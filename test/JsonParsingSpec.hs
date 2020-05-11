{-# LANGUAGE OverloadedStrings #-}
module JsonParsingSpec where

import Test.Hspec

import Data.Aeson
import Data.Aeson.Diff (diff,Patch(patchOperations))
import Data.ByteString.Lazy
import Data.Either (isRight)
import Data.Either.Combinators (fromRight')
import Data.Text
import Data.Text.Lazy.Encoding

import Network.Api.Postmark.Dmarc

spec :: Spec
spec = do
  describe "CreateRecord" $ do
    let fixture = encodeUtf8 "{\"email\":\"email@domain.com\",\"domain\":\"wildbit.com\"}"
    let decoded = eitherDecode fixture :: Either String CreateRecord
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "Record" $ do
    let fixture = encodeUtf8 "{\"domain\":\"postmarkapp.com\",\"public_token\":\"1mVgKNr5scA\",\"created_at\":\"2014-06-25T19:22:53Z\",\"reporting_uri\":\"mailto:randomhash+1mSgANr7scM@inbound.postmarkapp.com\",\"email\":\"tema@wildbit.com\"}"
    let decoded = eitherDecode fixture :: Either String Record
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "CreateRecordResponse" $ do
    let fixture = encodeUtf8 "{\"domain\":\"postmarkapp.com\",\"public_token\":\"1mVgKNr5scA\",\"created_at\":\"2014-06-25T19:22:53Z\",\"private_token\":\"005d8431-b020-41aa-230e-4d63a0357869\",\"reporting_uri\":\"mailto:randomhash+1mSgANr7scM@inbound.postmarkapp.com\",\"email\":\"tema@wildbit.com\"}"
    let decoded = eitherDecode fixture :: Either String CreateRecordResponse
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $ do
      let encoded = encode (fromRight' decoded)
      let a = fromRight' $ eitherDecode fixture
      let b = fromRight' $ eitherDecode encoded
      let p = diff a b
      patchOperations p `shouldBe` []

  describe "RecordUpdate" $ do
    let fixture = encodeUtf8 "{\"email\":\"email@domain.com\"}"
    let decoded = eitherDecode fixture :: Either String RecordUpdate
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DnsSnippet" $ do
    let fixture = encodeUtf8 "{\"value\":\"\\\"v=DMARC1; p=none; pct=100; rua=mailto:randomhash+1mSgKNr7scM@inbound.postmarkapp.com; sp=none; aspf=r;\\\"\",\"name\":\"_dmarc.wildbit.com.\"}"
    let decoded = eitherDecode fixture :: Either String DnsSnippet
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "VerifyDnsResponse" $ do
    let fixture = encodeUtf8 "{\"verified\":false}"
    let decoded = eitherDecode fixture :: Either String VerifyDnsResponse
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DmarcReportsListEntry" $ do
    let fixture = encodeUtf8 "{\"domain\":\"wildbit.com\",\"date_range_begin\":\"2014-04-27T20:00:00Z\",\"date_range_end\":\"2014-04-28T19:59:59Z\",\"id\":276,\"created_at\":\"2014-07-25T11:44:55Z\",\"external_id\":\"xxxxxxxxxxx\",\"organization_name\":\"google.com\"}"
    let decoded = eitherDecode fixture :: Either String DmarcReportsListEntry
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DmarcReportsListMeta" $ do
    let fixture = encodeUtf8 "{\"next\":276,\"next_url\":\"/records/my/reports?from_date=&to_date=&limit=1&after=276\",\"total\":2}"
    let decoded = eitherDecode fixture :: Either String DmarcReportsListMeta
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DmarcReportsListResponse" $ do
    let fixture = encodeUtf8 "{\"meta\":{\"next\":276,\"next_url\":\"/records/my/reports?from_date=&to_date=&limit=1&after=276\",\"total\":2},\"entries\":[{\"domain\":\"wildbit.com\",\"date_range_begin\":\"2014-04-27T20:00:00Z\",\"date_range_end\":\"2014-04-28T19:59:59Z\",\"id\":276,\"created_at\":\"2014-07-25T11:44:55Z\",\"external_id\":\"xxxxxxxxxxx\",\"organization_name\":\"google.com\"}]}"
    let decoded = eitherDecode fixture :: Either String DmarcReportsListResponse
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DmarcReportRecord" $ do
    let fixture = "{\"header_from\":\"wildbit.com\",\"source_ip\":\"127.0.0.1\",\"source_ip_version\":4,\"host_name\":\"example.org.\",\"count\":1,\"policy_evaluated_spf\":\"fail\",\"policy_evaluated_dkim\":\"fail\",\"policy_evaluated_disposition\":\"none\",\"policy_evaluated_reason_type\":null,\"spf_domain\":\"example.org\",\"spf_result\":\"pass\",\"dkim_domain\":null,\"dkim_result\":null}"
    let decoded = eitherDecode fixture :: Either String DmarcReportRecord
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "DmarcReport" $ do
    let fixture = "{\"id\":276,\"domain\":\"wildbit.com\",\"date_range_begin\":\"2014-04-27T20:00:00Z\",\"date_range_end\":\"2014-04-28T19:59:59Z\",\"source_uri\":\"mailto:noreply-dmarc-support@google.com\",\"external_id\":\"xxxxxxxxx\",\"email\":\"noreply-dmarc-support@google.com\",\"organization_name\":\"google.com\",\"created_at\":\"2014-07-25T11:44:55Z\",\"extra_contact_info\":\"http://support.google.com/a/bin/answer.py?answer=2466580\",\"records\":[{\"header_from\":\"wildbit.com\",\"source_ip\":\"127.0.0.1\",\"source_ip_version\":4,\"host_name\":\"example.org.\",\"count\":1,\"policy_evaluated_spf\":\"fail\",\"policy_evaluated_dkim\":\"fail\",\"policy_evaluated_disposition\":\"none\",\"policy_evaluated_reason_type\":null,\"spf_domain\":\"example.org\",\"spf_result\":\"pass\",\"dkim_domain\":null,\"dkim_result\":null}]}"
    let decoded = eitherDecode fixture :: Either String DmarcReport
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "RecoverApiTokenRequest" $ do
    let fixture = "{\"owner\":\"wildbit.com\"}"
    let decoded = eitherDecode fixture :: Either String RecoverApiTokenRequest
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "RecoverApiTokenResponse" $ do
    let fixture = "{\"recovery_initiated\":true}"
    let decoded = eitherDecode fixture :: Either String RecoverApiTokenResponse
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture

  describe "RotateApiTokenResponse" $ do
    let fixture = encodeUtf8 "{\"private_token\":\"115d8431-b020-41aa-230e-4d63a0357869\"}"
    let decoded = eitherDecode fixture :: Either String RotateApiTokenResponse
    it "parses successfully" $
      decoded `shouldSatisfy` isRight
    it "encodes symmetrically" $
      encode (fromRight' decoded) `shouldBe` fixture
