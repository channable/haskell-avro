{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Avro.Codec.TextSpec (spec) where

import           Avro.TestUtils
import           Data.ByteString.Lazy        (fromStrict)
import           Data.Text                   (Text)
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Test.Hspec

import           Data.Avro                   (decodeValueWithSchema, encodeValueWithSchema)
import           Data.Avro.Deriving          (deriveAvroFromByteString, r)
import           Data.Avro.Schema.ReadSchema (fromSchema)
import qualified Data.Avro.Schema.Schema     as Schema

{- HLINT ignore "Redundant do"        -}

deriveAvroFromByteString [r|
{
  "type": "record",
  "name": "OnlyText",
  "namespace": "test.contract",
  "fields": [ {"name": "onlyTextValue", "type": "string"} ]
}
|]

spec :: Spec
spec = describe "Avro.Codec.TextSpec" $ do
  let schema = schema'OnlyText
  let readSchema = fromSchema schema

  it "Derived the expected schema" $ require $ withTests 1 $ property $ do
    schema === Schema.Record
         { Schema.name = "test.contract.OnlyText"
         , Schema.aliases = []
         , Schema.doc = Nothing
         , Schema.fields =
             [ Schema.Field
                 { Schema.fldName = "onlyTextValue"
                 , Schema.fldAliases = []
                 , Schema.fldDoc = Nothing
                 , Schema.fldOrder = Just Schema.Ascending
                 , Schema.fldType = Schema.String Schema.NoLogicalType
                 , Schema.fldDefault = Nothing
                 }
             ]
         }

  it "Can decode \"This is an unit test\"" $ require $ withTests 1 $ property $ do
    -- The '(' here is the length (ASCII value) of the string
    let expectedBuffer = "(This is an unit test"
    let value = OnlyText "This is an unit test"
    encodeValueWithSchema schema value === expectedBuffer

  it "Can decode encoded Text values" $ require $ property $ do
    roundtripGen schema (OnlyText <$> Gen.text (Range.linear 0 128) Gen.alphaNum)

  it "Can process corrupted Text values without crashing" $ require $ property $ do
    bytes <- forAll $ Gen.bytes (Range.linear 0 511)
    eval $ decodeValueWithSchema @OnlyText readSchema (fromStrict bytes)
    success

  it "Ignores unknown logical types" $ require $ withTests 10 $ property $ do
    roundtripGen
      (Schema.String $ Schema.UnknownLogicalType "someunknowntype")
      (Gen.text (Range.linear 0 128) Gen.alphaNum)
