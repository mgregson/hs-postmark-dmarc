{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Main
Description: An example use of "Network.Api.Postmark.Dmarc"
-}
module Main where

import Control.Lens
import Data.Either.Combinators (fromRight')
import Data.Text (pack, unpack)
import Network.Api.Postmark.Dmarc as DMARC
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Environment
import System.Exit

main :: IO ()
main = do
  tokenR <- lookupEnv "POSTMARK_TOKEN"
  case tokenR of
    Nothing -> do
      putStrLn "To test interaction with the live API, provide an access token"
      putStrLn "in the environment variable POSTMARK_TOKEN."
    token -> do
      manager <- newManager tlsManagerSettings
      let env = mkClientEnv manager DMARC.baseUrl
      let client = authenticate DMARC.client $ fmap pack token
      r <- runClientM (getRecord client) env
      case r of
        Left (FailureResponse req res) -> do
          print $ responseBody res
          exitWith (ExitFailure 2)
        Right record -> do
          putStrLn $ "Found record for: " ++ (unpack $ record ^. domain)
          reportsList <- runClientM (getReportsList $ client) env >>= return.fromRight'
          print $ reportsList ^. meta
          let reportIds = map (^. DMARC.id) $ reportsList ^. entries
          reportsR <- mapM ((flip runClientM env).(getReport client)) reportIds
          let reports = map fromRight' reportsR
          print $ map (\r -> (r ^. organizationName, r ^. sourceUri,r ^. records)) reports
