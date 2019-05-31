{-# LANGUAGE LambdaCase, TemplateHaskell, DeriveGeneric, FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : PostgREST.Config
Description : Manages PostgREST configuration options.

This module provides a helper function to read the command line
arguments using the optparse-applicative and the AppConfig type to store
them.  It also can be used to define other middleware configuration that
may be delegated to some sort of external configuration.

It currently includes a hardcoded CORS policy but this could easly be
turned in configurable behaviour if needed.

Other hardcoded options such as the minimum version number also belong here.
-}
-- module PostgREST.Config ( prettyVersion
--                         , docsVersion
--                         , readOptions
--                         , corsPolicy
--                         , AppConfig (..)
--                         , configPoolTimeout'
--                         )
--        where
module PostgREST.Config where

import           Control.Applicative
import           Control.Monad                (fail)
import           Control.Lens                 (preview)
import           Crypto.JWT                   (StringOrURI,
                                               stringOrUri)
import GHC.Base (id)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BS
import qualified Data.CaseInsensitive         as CI
-- import qualified Data.Configurator            as C
-- import qualified Data.Configurator.Parser     as C
-- import           Data.Configurator.Types      as C
import           Data.List                    (lookup)
import           Data.Monoid
import           Data.Scientific              (floatingOrInteger)
import           Data.String                  (String)
import           Data.Text                    (dropWhileEnd, dropEnd,
                                               intercalate, lines,
                                               strip, take, splitOn)
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Text.IO                 (hPutStrLn)
import           Data.Version                 (versionBranch)
import           Development.GitRev           (gitHash)
import           Network.Wai
import           Network.Wai.Middleware.Cors  (CorsResourcePolicy (..))
import           Paths_postgrest              (version)
import           PostgREST.Parsers            (pRoleClaimKey)
import           PostgREST.Types              (ApiRequestError(..),
                                               JSPath, JSPathExp(..))
import           Protolude                    hiding (hPutStrLn, take,
                                               intercalate, (<>), Type)
import           System.IO                    (hPrint)
import           System.IO.Error              (IOError)
import           Text.Heredoc
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import qualified Text.PrettyPrint.ANSI.Leijen as L

import GHC.Natural (Natural)

import Options.Applicative (
    Parser, ParserInfo
  , customExecParser
  , info, help, fullDesc, progDesc, footerDoc
  , metavar, showHelpOnError, prefs, helper, strArgument
  )

import Data.Maybe (fromJust)
import Dhall (
    Interpret(..), InterpretOptions(..), Type, input
  , auto, autoWith, defaultInterpretOptions
  )

-- | Config file settings for the server
data AppConfigRaw = AppConfigRaw {
    raw_configDatabase          :: Text
  , raw_configAnonRole          :: Text
  , raw_configProxyUri          :: Maybe Text
  , raw_configSchema            :: Text
  , raw_configHost              :: Text
  , raw_configPort              :: Natural

  , raw_configJwtSecret         :: Maybe Text
  , raw_configJwtSecretIsBase64 :: Bool
  , raw_configJwtAudience       :: Maybe Text

  , raw_configPool              :: Natural
  , raw_configPoolTimeout       :: Natural
  , raw_configMaxRows           :: Maybe Natural
  , raw_configReqCheck          :: Maybe Text
  , raw_configQuiet             :: Bool
  , raw_configSettings          :: [[Text]]
  , raw_configRoleClaimKey      :: Maybe Text
  , raw_configExtraSearchPath   :: [Text]
  } deriving (Generic, Show)
instance Interpret AppConfigRaw

data AppConfig = AppConfig {
    configDatabase          :: Text
  , configAnonRole          :: Text
  , configProxyUri          :: Maybe Text
  , configSchema            :: Text
  , configHost              :: Text
  , configPort              :: Int

  , configJwtSecret         :: Maybe ByteString
  , configJwtSecretIsBase64 :: Bool
  , configJwtAudience       :: Maybe StringOrURI

  , configPool              :: Int
  , configPoolTimeout       :: Int
  , configMaxRows           :: Maybe Integer
  , configReqCheck          :: Maybe Text
  , configQuiet             :: Bool
  , configSettings          :: [(Text, Text)]
  , configRoleClaimKey      :: Either ApiRequestError JSPath
  , configExtraSearchPath   :: [Text]
  }

mkAppConfig :: AppConfigRaw -> AppConfig
mkAppConfig AppConfigRaw { .. } = do
  let configDatabase = raw_configDatabase
  let configAnonRole = raw_configAnonRole
  let configProxyUri = raw_configProxyUri
  let configSchema = raw_configSchema
  let configHost = raw_configHost
  let configPort = fromIntegral raw_configPort
  let configJwtSecret =  toS <$> raw_configJwtSecret
  let configJwtSecretIsBase64 = raw_configJwtSecretIsBase64
  let configJwtAudience = raw_configJwtAudience >>=
                            preview stringOrUri >>= \case 
                              "" -> Nothing
                              a -> Just a
                               
  let configPool = fromIntegral raw_configPool
  let configPoolTimeout = fromIntegral raw_configPoolTimeout
  let configMaxRows = fromIntegral <$> raw_configMaxRows
  let configReqCheck = raw_configReqCheck
  let configQuiet = raw_configQuiet
  let configSettings = map (\(k:v:[]) -> (k,v)) raw_configSettings
  let configRoleClaimKey = maybe (Right [JSPKey "role"]) pRoleClaimKey raw_configRoleClaimKey
  let configExtraSearchPath = raw_configExtraSearchPath
  AppConfig { .. }

instance Interpret (Either ApiRequestError JSPath) where
  autoWith opt = (pRoleClaimKey :: Text -> Either ApiRequestError JSPath) <$> autoWith opt

configPoolTimeout' :: (Fractional a) => AppConfig -> a
configPoolTimeout' =
  fromRational . toRational . configPoolTimeout


defaultCorsPolicy :: CorsResourcePolicy
defaultCorsPolicy =  CorsResourcePolicy Nothing
  ["GET", "POST", "PATCH", "PUT", "DELETE", "OPTIONS"] ["Authorization"] Nothing
  (Just $ 60*60*24) False False True

-- | CORS policy to be used in by Wai Cors middleware
corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy req = case lookup "origin" headers of
  Just origin -> Just defaultCorsPolicy {
      corsOrigins = Just ([origin], True)
    , corsRequestHeaders = "Authentication":accHeaders
    , corsExposedHeaders = Just [
        "Content-Encoding", "Content-Location", "Content-Range", "Content-Type"
      , "Date", "Location", "Server", "Transfer-Encoding", "Range-Unit"
      ]
    }
  Nothing -> Nothing
  where
    headers = requestHeaders req
    accHeaders = case lookup "access-control-request-headers" headers of
      Just hdrs -> map (CI.mk . toS . strip . toS) $ BS.split ',' hdrs
      Nothing -> []

-- | User friendly version number
prettyVersion :: Text
prettyVersion =
  intercalate "." (map show $ versionBranch version)
  <> " (" <> take 7 $(gitHash) <> ")"

-- | Version number used in docs
docsVersion :: Text
docsVersion = "v" <> dropEnd 1 (dropWhileEnd (/= '.') prettyVersion)

readOptions :: IO AppConfig
readOptions = do
  -- First read the config file path from command line
  cfgPath <- customExecParser (prefs showHelpOnError) progOpts
  mkAppConfig <$> input (autoWith defaultInterpretOptions {fieldModifier = configFM}) (toS cfgPath)

-- | Function to read and parse options from the command line
--readOptions :: IO AppConfig
--readOptions = do
--  
--  cfgPath <- customExecParser parserPrefs opts
--  -- Now read the actual config file
--  conf <- catch
--    (C.readConfig =<< C.load [C.Required cfgPath])
--    configNotfoundHint
--
--  let (mAppConf, errs) = flip C.runParserM conf $
--        AppConfig
--          <$> C.key "db-uri"
--          <*> C.key "db-anon-role"
--          <*> (mfilter (/= "") <$> C.key "server-proxy-uri")
--          <*> C.key "db-schema"
--          <*> (fromMaybe "127.0.0.1" . mfilter (/= "") <$> C.key "server-host")
--          <*> (fromMaybe 3000 . join . fmap coerceInt <$> C.key "server-port")
--          <*> (fmap encodeUtf8 . mfilter (/= "") <$> C.key "jwt-secret")
--          <*> (fromMaybe False . join . fmap coerceBool <$> C.key "secret-is-base64")
--          <*> parseJwtAudience "jwt-aud"
--          <*> (fromMaybe 10 . join . fmap coerceInt <$> C.key "db-pool")
--          <*> (fromMaybe 10 . join . fmap coerceInt <$> C.key "db-pool-timeout")
--          <*> (join . fmap coerceInt <$> C.key "max-rows")
--          <*> (mfilter (/= "") <$> C.key "pre-request")
--          <*> pure False
--          <*> (fmap (fmap coerceText) <$> C.subassocs "app.settings")
--          <*> (maybe (Right [JSPKey "role"]) parseRoleClaimKey <$> C.key "role-claim-key")
--          <*> (maybe ["public"] splitExtraSearchPath <$> C.key "db-extra-search-path")
--
--  case mAppConf of
--    Nothing -> do
--      forM_ errs $ hPrint stderr
--      exitFailure
--    Just appConf ->
--      return appConf
--
--  where
--    parseJwtAudience :: Name -> C.ConfigParserM (Maybe StringOrURI)
--    parseJwtAudience k =
--      C.key k >>= \case
--        Nothing -> pure Nothing -- no audience in config file
--        Just aud -> case preview stringOrUri (aud :: String) of
--          Nothing -> fail "Invalid Jwt audience. Check your configuration."
--          (Just "") -> pure Nothing
--          aud' -> pure aud'
--
--    coerceText :: Value -> Text
--    coerceText (String s) = s
--    coerceText v = show v
--
--    coerceInt :: (Read i, Integral i) => Value -> Maybe i
--    coerceInt (Number x) = rightToMaybe $ floatingOrInteger x
--    coerceInt (String x) = readMaybe $ toS x
--    coerceInt _          = Nothing
--
--    coerceBool :: Value -> Maybe Bool
--    coerceBool (Bool b)   = Just b
--    coerceBool (String b) = readMaybe $ toS b
--    coerceBool _          = Nothing
--
--    parseRoleClaimKey :: Value -> Either ApiRequestError JSPath
--    parseRoleClaimKey (String s) = pRoleClaimKey s
--    parseRoleClaimKey v = pRoleClaimKey $ show v
--
--    splitExtraSearchPath :: Value -> [Text]
--    splitExtraSearchPath (String s) = strip <$> splitOn "," s
--    splitExtraSearchPath _ = []
--
--
--    parserPrefs = prefs showHelpOnError
--
--    configNotfoundHint :: IOError -> IO a
--    configNotfoundHint e = do
--      hPutStrLn stderr $
--        "Cannot open config file:\n\t" <> show e
--      exitFailure
--
--

exampleCfg :: Text
exampleCfg = 
  [str|{
      |    db-uri = ""
      |  , db-anon-role = ""
      |  , server-proxy-uri = None Text
      |    {-- this schema gets added to the search_path of every request --}
      |  , db-schema = "public"
      |  , server-host = "127.0.0.1"
      |  , server-port = 3000
      |
      |  , jwt-secret = None Text
      |  , secret-is-base64 = False
      |  , jwt-aud = None Text
      |
      |  , db-pool = 10
      |  , db-pool-timeout = 10
      |  , max-rows = Some 1000
      |  , pre-request = None Text
      |  , quiet = False
      |  , app-settings = [] : List (List Text)
      |  , role-claim-key = None Text
      |  , db-extra-search-path = ["extensions"]
      |} // {
      |    db-uri = "postgres://user:pass@localhost:5432/dbname"
      |  , db-schema =  "public"
      |  , db-anon-role = "user"
      |  , server-host = "192.168.1.1"
      |  , server-port = 2222
      |  {--
      |  , server-proxy-uri = Optional "http://localhost:8118" 
      |  , jwt-secret = "foo"
      |  , secret-is-base64= True 
      |  , jwt-aud = "your_audience_claim" 
      |
      |  , max-rows = 10000 
      |  
      |  , pre-request = "stored_proc_name"
      |
      |  , role-claim-key = ".role" 
      |  , db-extra-search-path = ["extensions", "util"]
      |  --}
      |  }
      |]

pathParser :: Parser FilePath
pathParser =
  strArgument $
    metavar "FILENAME" <>
    help "Path to configuration file"

progOpts :: ParserInfo FilePath
progOpts = info (helper <*> pathParser) $
         fullDesc
         <> progDesc (
             "PostgREST "
             <> toS prettyVersion
             <> " / create a REST API to an existing Postgres database"
           )
         <> footerDoc (Just $
             text "Example Config File:"
             L.<> nest 2 (hardline L.<> (vsep . map (text . toS) . lines $ exampleCfg))
           )

configFM :: Text -> Text
configFM = \case
  "raw_configDatabase" -> "db-uri"
  "raw_configAnonRole" -> "db-anon-role"
  "raw_configProxyUri" -> "server-proxy-uri"
  "raw_configSchema" -> "db-schema"
  "raw_configHost" -> "server-host"
  "raw_configPort" -> "server-port"
  "raw_configJwtSecret" -> "jwt-secret"
  "raw_configJwtSecretIsBase64" -> "secret-is-base64"
  "raw_configJwtAudience" -> "jwt-aud"
  "raw_configPool" -> "db-pool"
  "raw_configPoolTimeout" -> "db-pool-timeout"
  "raw_configMaxRows" -> "max-rows"
  "raw_configReqCheck" -> "pre-request"
  "raw_configQuiet" -> "quiet"
  "raw_configSettings" -> "app-settings"
  "raw_configRoleClaimKey" -> "role-claim-key"
  "raw_configExtraSearchPath" -> "db-extra-search-path"
  a -> a

repl :: IO ()
repl = do
  x <- input (autoWith defaultInterpretOptions {fieldModifier = configFM}) exampleCfg
  print (x :: AppConfigRaw)
