{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UpCheck
  ( upCheck,
  )
where

import Control.Exception
import Control.Monad
import Control.Retry
import Data.Aeson.Types as JSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import System.Environment
import System.Exit
import Test.Syd
import Test.Syd.OptParse (defaultSettings)
import YamlParse.Applicative

upCheck :: IO ()
upCheck = do
  args <- getArgs
  case args of
    [] -> die "Supply a spec file path as an argument"
    (sfp : _) -> do
      spec <- decodeFileThrow sfp
      runCheckSpec spec

runCheckSpec :: CheckSpec -> IO ()
runCheckSpec cs =
  sydTestWith defaultSettings (checkSpec cs)

checkSpec :: CheckSpec -> Spec
checkSpec CheckSpec {..} =
  aroundAll
    ( \func -> do
        man <- HTTP.newTlsManager
        func man
    )
    $ mapM_ singleCheckSpec specChecks

singleCheckSpec :: Check -> TestDef '[HTTP.Manager] ()
singleCheckSpec =
  let uRIIt :: URI -> String -> (HTTP.Manager -> IO ()) -> TestDef '[HTTP.Manager] ()
      uRIIt d s = itWithOuter (unwords [s, show d])
   in \case
        CheckGet uri mExpectedStatus mExpectedLocation ->
          uRIIt uri "GET" $ \man -> do
            req <- requestFromURI uri
            errOrResp <-
              retryHTTP $
                (() <$) <$> httpRaw req man
            case errOrResp of
              Left err -> do
                let ctx = unlines ["Request: ", ppShow req]
                context ctx $
                  expectationFailure $
                    show err
              Right resp -> do
                let ctx = unlines ["Request: ", ppShow req, "Response: ", ppShow resp]
                context ctx $ do
                  forM_ mExpectedStatus $ \expectedStatus ->
                    HTTP.statusCode (HTTP.responseStatus resp) `shouldBe` expectedStatus
                  forM_ mExpectedLocation $ \expectedLocation ->
                    case lookup "Location" (responseHeaders resp) of
                      Nothing -> expectationFailure $ "No location header found in response, but expected: " <> show expectedLocation
                      Just loc -> case parseURI (T.unpack (TE.decodeUtf8 loc)) of
                        Nothing -> expectationFailure $ "Found a location header, but it didn't parse as a URI: " <> show loc
                        Just actualLoc ->
                          actualLoc `shouldBe` expectedLocation

retryHTTP :: IO (Response a) -> IO (Either HttpException (Response a))
retryHTTP action = retrying policy (\_ e -> pure (couldBeFlaky e)) (\_ -> (Right <$> action) `catch` (pure . Left))
  where
    couldBeFlaky (Left e) = case e of -- TODO
      HttpExceptionRequest _ hec -> case hec of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        ConnectionFailure _ -> True
        NoResponseDataReceived -> True
        _ -> False
      InvalidUrlException _ _ -> False
    couldBeFlaky _ = False
    policy = exponentialBackoff 100 <> limitRetries 10

data CheckSpec = CheckSpec
  { specChecks :: ![Check]
  }
  deriving (Show, Eq, Generic)

instance FromJSON CheckSpec where
  parseJSON = viaYamlSchema

instance YamlSchema CheckSpec where
  yamlSchema =
    objectParser "CheckSpec" $
      CheckSpec <$> requiredField "checks" "The checks to perform"

data Check
  = CheckGet !URI (Maybe Int) (Maybe URI)
  deriving (Show, Eq, Generic)

instance FromJSON Check where
  parseJSON = viaYamlSchema

instance YamlSchema Check where
  yamlSchema =
    objectParser "Check" $
      CheckGet
        <$> requiredFieldWith "get" "The URL to GET" (maybeParser parseURI yamlSchema)
        <*> optionalField "status" "The expected status code. If this is not supplied, any status code will pass the test, as long as the server replied."
        <*> optionalFieldWith "location" "The expected location for a redirect" (maybeParser parseURI yamlSchema)
