{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UpCheck
  ( upCheck,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Exception
import Control.Monad
import Control.Retry
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Network.URI
import Path
import Path.IO
import Paths_upcheck
import System.Environment
import System.Exit
import Test.Syd
import Test.Syd.OptParse (defaultSettings)

upCheck :: IO ()
upCheck = do
  args <- getArgs
  case args of
    [] -> die "Supply a spec file path as an argument"
    (sfp : _) -> do
      afp <- resolveFile' sfp
      mspec <- readYamlConfigFile afp
      case mspec of
        Nothing -> die $ "Spec file not found: " <> fromAbsFile afp
        Just spec -> runCheckSpec spec

runCheckSpec :: CheckSpec -> IO ()
runCheckSpec cs = sydTestWith defaultSettings (checkSpec cs)

checkSpec :: CheckSpec -> Spec
checkSpec CheckSpec {..} =
  aroundAll
    ( \func -> do
        man <- HTTP.newTlsManager
        func man
    )
    $ doNotRandomiseExecutionOrder
    $ mapM_ (singleCheckSpec specRetryPolicy) specChecks

singleCheckSpec :: RetryPolicySpec -> Check -> TestDef '[HTTP.Manager] ()
singleCheckSpec retryPolicySpec =
  let uRIIt :: URI -> String -> (HTTP.Manager -> IO ()) -> TestDef '[HTTP.Manager] ()
      uRIIt d s = itWithOuter (unwords [s, show d])
   in \case
        CheckGet uri mExpectedStatus mExpectedLocation ->
          uRIIt uri "GET" $ \man -> do
            req' <- requestFromURI uri
            let req = req' {requestHeaders = ("User-Agent", TE.encodeUtf8 $ T.pack $ "upcheck-" <> showVersion version) : requestHeaders req'}
            errOrResp <-
              retryHTTP retryPolicySpec req $
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
                      Just loc -> case parseURIReference (T.unpack (TE.decodeUtf8 loc)) of
                        Nothing -> expectationFailure $ "Found a location header, but it didn't parse as a URI: " <> show loc
                        Just actualLoc ->
                          actualLoc `shouldBe` expectedLocation

retryHTTP ::
  RetryPolicySpec ->
  -- | Just  for the error message
  Request ->
  IO (Response a) ->
  IO (Either HttpException (Response a))
retryHTTP retryPolicySpec req action =
  retrying
    (retryPolicySpecToRetryPolicy retryPolicySpec)
    (\_ e -> pure (couldBeFlaky e))
    ( \_ ->
        (Right <$> action)
          `catches` [ Handler $ pure . Left,
                      Handler $ pure . Left . toHttpException req
                    ]
    )
  where
    couldBeFlaky (Left e) = case e of
      HttpExceptionRequest _ hec -> case hec of
        ResponseTimeout -> True
        ConnectionTimeout -> True
        ConnectionFailure _ -> True
        NoResponseDataReceived -> True
        _ -> False
      InvalidUrlException _ _ -> False
    couldBeFlaky _ = False

data CheckSpec = CheckSpec
  { specRetryPolicy :: !RetryPolicySpec,
    specChecks :: ![Check]
  }

instance HasCodec CheckSpec where
  codec =
    object "CheckSpec" $
      CheckSpec
        <$> optionalFieldWithDefault "retry-policy" defaultRetryPolicySpec "The retry policy for flaky checks due to network failures etc"
          .= specRetryPolicy
        <*> requiredField "checks" "The checks to perform"
          .= specChecks

data RetryPolicySpec = RetryPolicySpec
  { retryPolicySpecMaxRetries :: !Word,
    retryPolicySpecBaseDelay :: !Word
  }

instance HasCodec RetryPolicySpec where
  codec =
    object "RetryPolicySpec" $
      RetryPolicySpec
        <$> optionalFieldWithDefault "max-retries" (retryPolicySpecMaxRetries defaultRetryPolicySpec) "The maximum number of retries"
          .= retryPolicySpecMaxRetries
        <*> optionalFieldWithDefault "base-delay" (retryPolicySpecBaseDelay defaultRetryPolicySpec) "The delay between the first and second try, in microseconds"
          .= retryPolicySpecBaseDelay

defaultRetryPolicySpec :: RetryPolicySpec
defaultRetryPolicySpec =
  RetryPolicySpec
    { retryPolicySpecMaxRetries = 10,
      retryPolicySpecBaseDelay = 100_000 -- 100 ms
    }

retryPolicySpecToRetryPolicy :: RetryPolicySpec -> RetryPolicyM IO
retryPolicySpecToRetryPolicy RetryPolicySpec {..} =
  mconcat
    [ exponentialBackoff $ fromIntegral retryPolicySpecBaseDelay,
      limitRetries $ fromIntegral retryPolicySpecMaxRetries
    ]

data Check = CheckGet
  { checkURI :: !URI,
    checkStatus :: !(Maybe Int),
    checkLocation :: !(Maybe URI)
  }

instance HasCodec Check where
  codec =
    object "Check" $
      CheckGet
        <$> requiredField "get" "The URL to GET"
          .= checkURI
        <*> optionalField "status" "The expected status code. If this is not supplied, any status code will pass the test, as long as the server replied."
          .= checkStatus
        <*> optionalField "location" "The expected location for a redirect"
          .= checkLocation

instance HasCodec URI where
  codec =
    bimapCodec
      ( \s -> case parseURIReference s of
          Nothing -> Left $ "Could not parse URI: " <> s
          Just uri -> Right uri
      )
      show
      codec
