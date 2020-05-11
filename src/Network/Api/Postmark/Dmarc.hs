{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module: Network.Api.Postmark.Dmarc
Description: A wrapper for
  [Postmark’s DMARC REST API](https://dmarc.postmarkapp.com/api/) build on top
  of "Servant.Client".
Maintainer: mgregson@gregson.io
Stability: experimental
Copyright: MIT

Provides an interface to [Postmark's DMARC REST API](https://dmarc.postmarkapp.com/api/).
Most types have class lenses and member field lenses. See the @example/@
directory for samples of use.
-}
module Network.Api.Postmark.Dmarc where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.TH
import qualified Data.Char as C (toLower)
import Data.HashMap.Strict (insert)
import Data.Proxy
import Data.Text as T hiding (drop, toLower, map)
import Data.Time.LocalTime
import GHC.Generics
import Servant.API
import Servant.Client as SC

-- | The unique identifier for each DMARC report.
type Id = Int

type IP = Text
type Domain = Text
type Email = Text
type URI = Text

-- | The API tokens used to access the Postmark API.
type ApiToken = Text

{-| An enumeration of the IP address version that might be returned in the API
objects.
-}
data IPVersion =
  IPv4 -- ^ IP address version 4
  | IPv6 -- ^ IP address version 6
  deriving (Generic, Show)

instance FromJSON IPVersion where
  parseJSON = withScientific "IPVersion" $ \n ->
    case n of
      4 -> return IPv4
      6 -> return IPv6
      _ -> error "Unable to parse IP protocol version"

instance ToJSON IPVersion where
  toJSON IPv4 = Number 4
  toJSON IPv6 = Number 6

{-| The input to the "Create a record" endpoint.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#create-a-record)
-}
data CreateRecord = CreateRecord
  { _createRecordEmail                           :: Email
  -- ^ The email address to send DMARC report summaries to.
  , _createRecordDomain                          :: Domain
  -- ^ The domain name to produce DMARC report summaries for.
  } deriving (Show)
makeClassy ''CreateRecord
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 13) }) ''CreateRecord
makeFields ''CreateRecord

{-| The configuration for a DMARC record being monitored by Postmark. There's
one record for each API key.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#get-a-record)
-}
data Record = Record
  { _recordDomain                                :: Domain
  , _recordPublicToken                           :: Text
  , _recordCreatedAt                             :: ZonedTime
  , _recordReportingUri                          :: Network.Api.Postmark.Dmarc.URI
  , _recordEmail                                 :: Email
  } deriving (Show)
makeClassy ''Record
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 7) }) ''Record
makeFields ''Record

{-| The response from the new record creation endpoint. Includes the 'Record'
along with an 'ApiToken' that can be used to query the reports for the domain
or manage the configuration.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#get-a-record)

/This is the only type that does not currently have field lenses./
-}
data CreateRecordResponse = CreateRecordResponse
  { _createRecordResponseRecord                  :: Record
  , _createRecordResponsePrivateToken            :: ApiToken
  } deriving (Show)
makeClassy ''CreateRecordResponse
--makeFields ''CreateRecordResponse

instance FromJSON CreateRecordResponse where
  parseJSON d = flip (withObject "CreateRecordResponse") d $ \o -> do
    token <- o .: "private_token"
    record <- parseJSON d
    return $ CreateRecordResponse record token

instance ToJSON CreateRecordResponse where
  toJSON (CreateRecordResponse r t) =
    case toJSON r of
      Object o -> Object (insert "private_token" (toJSON t) o)
      _ -> error "unable to serialize"

{-| An update to make to an existing configuration. The only editable attribute
of a configuration is the @email@ attribute.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#update-a-record)
-}
data RecordUpdate = RecordUpdate
  { _recordUpdateEmail                           :: Email
  } deriving (Show)
makeClassy ''RecordUpdate
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 13) }) ''RecordUpdate
makeFields ''RecordUpdate

{-| The structure returned by the DNS snippet lookup endpoint.

Contains the raw TXT record data containing the DMARC reporting rules from the
domain's DNS zone, as well as the name of the DNS record.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#get-dns-snippet)
-}
data DnsSnippet = DnsSnippet
  { _dnsSnippetValue                             :: Text
  , _dnsSnippetName                              :: Text
  } deriving (Show)
makeClassy ''DnsSnippet
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 11) }) ''DnsSnippet
makeFields ''DnsSnippet

{-| The response returned by the DNS verification endpoint.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#verify-dns)
-}
data VerifyDnsResponse = VerifyDnsResponse
  { _verifyDnsResponseVerified                   :: Bool
  } deriving (Show)
makeClassy ''VerifyDnsResponse
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 18) }) ''VerifyDnsResponse
makeFields ''VerifyDnsResponse

{-| An entry in the list of DMARC reports returned from the API.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#list-dmarc-reports)
-}
data DmarcReportsListEntry = DmarcReportsListEntry
  { _dmarcReportsListEntryDomain                 :: Domain
  , _dmarcReportsListEntryDateRangeBegin         :: ZonedTime
  , _dmarcReportsListEntryDateRangeEnd           :: ZonedTime
  , _dmarcReportsListEntryId                     :: Id
  , _dmarcReportsListEntryCreatedAt              :: ZonedTime
  , _dmarcReportsListEntryExternalId             :: Text
  , _dmarcReportsListEntryOrganizationName       :: Text
  } deriving (Show)
makeClassy ''DmarcReportsListEntry
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 22) }) ''DmarcReportsListEntry
makeFields ''DmarcReportsListEntry

{-| Metadata returned from the DMARC reports list API endpoint.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#list-dmarc-reports)
-}
data DmarcReportsListMeta = DmarcReportsListMeta
  { _dmarcReportsListMetaNext                    :: Maybe Int
  , _dmarcReportsListMetaNextUrl                 :: Maybe Text
  , _dmarcReportsListMetaTotal                   :: Int
  } deriving (Show)
makeClassy ''DmarcReportsListMeta
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 21) }) ''DmarcReportsListMeta
makeFields ''DmarcReportsListMeta

{-| The full response from the Postmart DMARC reports list API endpoint.
Contains both the partial list of entries and metadata describing the full
response set.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#list-dmarc-reports)
-}
data DmarcReportsListResponse = DmarcReportsListResponse
  { _dmarcReportsListResponseMeta                :: DmarcReportsListMeta
  , _dmarcReportsListResponseEntries             :: [DmarcReportsListEntry]
  } deriving (Show)
makeClassy ''DmarcReportsListResponse
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 25) }) ''DmarcReportsListResponse
makeFields ''DmarcReportsListResponse

{-| An individual record in a particular DMARC report.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#get-a-report-by-id)
-}
data DmarcReportRecord = DmarcReportRecord
  { _dmarcReportRecordHeaderFrom                 :: Text
  , _dmarcReportRecordSourceIp                   :: IP
  , _dmarcReportRecordSourceIpVersion            :: IPVersion
  , _dmarcReportRecordHostName                   :: Text
  , _dmarcReportRecordCount                      :: Int
  , _dmarcReportRecordPolicyEvaluatedSpf         :: Maybe Text
  , _dmarcReportRecordPolicyEvaluatedDkim        :: Maybe Text
  , _dmarcReportRecordPolicyEvaluatedDisposition :: Maybe Text
  , _dmarcReportRecordPolicyEvaluatedReasonType  :: Maybe Text
  , _dmarcReportRecordSpfDomain                  :: Maybe Text
  , _dmarcReportRecordSpfResult                  :: Maybe Text
  , _dmarcReportRecordDkimDomain                 :: Maybe Text
  , _dmarcReportRecordDkimResult                 :: Maybe Text
  } deriving (Show)
makeClassy ''DmarcReportRecord
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 18) }) ''DmarcReportRecord
makeFields ''DmarcReportRecord

{-| An individual DMARC report from the Postmark API.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#get-a-report-by-id)
-}
data DmarcReport = DmarcReport
  { _dmarcReportId                               :: Id
  , _dmarcReportDomain                           :: Domain
  , _dmarcReportDateRangeBegin                   :: ZonedTime
  , _dmarcReportDateRangeEnd                     :: ZonedTime
  , _dmarcReportSourceUri                        :: Network.Api.Postmark.Dmarc.URI
  , _dmarcReportExternalId                       :: Text
  , _dmarcReportEmail                            :: Email
  , _dmarcReportOrganizationName                 :: Text
  , _dmarcReportCreatedAt                        :: ZonedTime
  , _dmarcReportExtraContactInfo                 :: Text
  , _dmarcReportRecords                          :: [DmarcReportRecord]
  } deriving (Show)
makeClassy ''DmarcReport
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 12) }) ''DmarcReport
makeFields ''DmarcReport

{-| The request object to trigger the token recovery process.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#recover-api-token)
-}
data RecoverApiTokenRequest = RecoverApiTokenRequest
  { _recordApiTokenRequestOwner                  :: Text
  } deriving (Show)
makeClassy ''RecoverApiTokenRequest
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 22) }) ''RecoverApiTokenRequest
makeFields ''RecoverApiTokenRequest

{-| The response object returned when a token recovery process is requested.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#recover-api-token)
-}
data RecoverApiTokenResponse = RecoverApiTokenResponse
  { _recoverApiTokenResponseRecoveryInitiated    :: Bool
  } deriving (Show)
makeClassy ''RecoverApiTokenResponse
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 24) }) ''RecoverApiTokenResponse
makeFields ''RecoverApiTokenResponse

{-| The response returned from the API when token rotation is requested.

[Postmark Documentation](https://dmarc.postmarkapp.com/api/#rotate-api-token)
-}
data RotateApiTokenResponse = RotateApiTokenResponse
  { _rotateApiTokenResponsePrivateToken          :: ApiToken
  } deriving (Show)
makeClassy ''RotateApiTokenResponse
deriveJSON (defaultOptions { fieldLabelModifier = snakeCase.(drop 23) }) ''RotateApiTokenResponse
makeFields ''RotateApiTokenResponse

{-| Definition of the authenticated API endpoints using the "Servant" type
syntax.
-}
type AuthenticatedApi =
  Get '[JSON] Record
  :<|> ReqBody '[JSON] RecordUpdate :> Patch '[JSON] Record
  :<|> Delete '[JSON] NoContent
  :<|> "dns" :> Get '[JSON] DnsSnippet
  :<|> "verify" :> Post '[JSON] VerifyDnsResponse
  :<|> "reports" :> Get '[JSON] DmarcReportsListResponse
  :<|> "reports" :> Capture "reportId" Id :> Get '[JSON] DmarcReport
  :<|> "token" :> "rotate" :> Post '[JSON] RotateApiTokenResponse

{-| Complete API definition in the "Servant" type syntax.
-}
type PostmarkDmarcApi =
  "records" :> ReqBody '[JSON] CreateRecord :> Post '[JSON] CreateRecordResponse
  :<|> "records" :> "my" :> Header "X-Api-Token" ApiToken :> AuthenticatedApi
  :<|> "token" :> "recover" :> ReqBody '[JSON] RecoverApiTokenRequest :> Post '[JSON] RecoverApiTokenResponse

postmarkDmarcApi :: Proxy PostmarkDmarcApi
postmarkDmarcApi = Proxy

{-| An interface to the authenticated portion of the Postmark DMARC REST API.
Create an instance by calling 'authenticate' on a 'PostmarkDmarcApiClient'.
-}
data PostmarkDmarcAuthenticatedApiClient = PostmarkDmarcAuthenticatedApiClient
  { getRecord :: ClientM Record
  -- ^ Retrieve the record configuration associated with the current API token.
  , updateRecord :: RecordUpdate -> ClientM Record
  -- ^ Update the record configuration associated with the current API token.
  , deleteRecord :: ClientM NoContent
  -- ^ Delete the record associated with the current API token.
  , getDnsSnippet :: ClientM DnsSnippet
  -- ^ Get the DNS entry associated with the domain managed by the current API token.
  , verifyDns :: ClientM VerifyDnsResponse
  -- ^ Request verification of the DNS entry associated with the current API token.
  , getReportsList :: ClientM DmarcReportsListResponse
  -- ^ Get a list of DMARC reports for the current API token. See 'getReport' to
  -- get an individual report.
  , getReport :: Int -> ClientM DmarcReport
  -- ^ Get an individual DMARC report for the current API token. See
  -- 'getReportsList' to get a list of available reports.
  , rotateToken :: ClientM RotateApiTokenResponse
  -- ^ Generate a new API token to replace the current one.
  }

data PostmarkDmarcApiClient = PostmarkDmarcApiClient
  { createNewRecord :: CreateRecord -> ClientM CreateRecordResponse
  -- ^ Register a new domain with the Postmark DMARC REST API.
  , authenticate :: Maybe ApiToken -> PostmarkDmarcAuthenticatedApiClient
  -- ^ Get a client reference for the authentication portion of the API.
  , recoverToken :: RecoverApiTokenRequest -> ClientM RecoverApiTokenResponse
  -- ^ Initiate a token recovery process.
  }

{-| A client definition for the Postmark DMARC API.

This is the main entrypoint for most use cases.

To create a new record:

@
import Network.Api.Postmark.Dmarc
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
main = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager baseUrl
  result <- runClientM createNewRecord client $ CreateRecord "me@email.com" "google.com"
  putStrLn result
@
-}
client :: PostmarkDmarcApiClient
client = PostmarkDmarcApiClient{..}
  where
    createNewRecord :<|> authenticatedClient :<|> recoverToken = SC.client postmarkDmarcApi

    authenticate apiToken = PostmarkDmarcAuthenticatedApiClient{..}
      where
        getRecord :<|> updateRecord :<|> deleteRecord :<|> getDnsSnippet :<|> verifyDns :<|> getReportsList :<|> getReport :<|> rotateToken = authenticatedClient apiToken

baseUrl = BaseUrl Https "dmarc.postmarkapp.com" 443 ""
