{-# LANGUAGE OverloadedStrings #-}

module Avro.Encoding.LogicalTypesSpec
where

import Avro.Data.Logical
import Data.Avro                   (decodeValue, decodeValueWithSchema, encodeValue, encodeValueWithSchema)
import Data.Avro.Schema.ReadSchema (fromSchema)
import qualified Data.Aeson as A
import qualified Data.Avro.Schema.Schema as S

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Avro.Encoding.LogicalTypesSpec" $ do
  describe "Round-tripping" $ do
    it "should encode with ToAvro and decode with FromAvro" $ require $ property $ do
      x <- forAll logicalGen
      tripping x (encodeValueWithSchema schema'Logical) (decodeValueWithSchema (fromSchema schema'Logical))

    it "should encode/decode value using HasAvroSchema" $ require $ property $ do
      x <- forAll logicalGen
      tripping x encodeValue decodeValue

  describe "derived schema" $ do
    it "has the expected field types" $ require $ withTests 1 $ property $ do
      let fieldTypes = [ (S.fldName f, S.fldType f) | f <- S.fields schema'Logical]
      fieldTypes ===
        [ ( "noLogicalType1"
          , S.String { S.logicalTypeS = S.NoLogicalType }
          )
        , ( "noLogicalType2"
          , S.String { S.logicalTypeS = S.NoLogicalType }
          )
        , ( "someUnknownLogicalType"
          , S.String { S.logicalTypeS = S.UnknownLogicalType "logical-address" }
          )
        , ( "logicalTypeOnWrongType"
            -- The "timestamp-millis" logical type only exist for 'long' types, so for strings it
            -- counts as unknown.
          , S.String { S.logicalTypeS = S.UnknownLogicalType "timestamp-millis" }
          )
        , ( "tsMillis"
          , S.Long { S.logicalTypeL = S.KnownLogicalType S.TimestampMillis }
          )
        , ( "tsMicros"
          , S.Long { S.logicalTypeL = S.KnownLogicalType S.TimestampMicros }
          )
        , ( "timeMillis"
          , S.Int { S.logicalTypeI = S.KnownLogicalType S.TimeMillis }
          )
        , ( "timeMicros"
          , S.Long { S.logicalTypeL = S.KnownLogicalType S.TimeMicros }
          )
        , ( "localTimestampMillis"
          , S.Long { S.logicalTypeL = S.KnownLogicalType S.LocalTimestampMillis }
          )
        , ( "localTimestampMicros"
          , S.Long { S.logicalTypeL = S.KnownLogicalType S.LocalTimestampMicros }
          )
        ]

    it "converts to the expected json" $ require $ withTests 1 $ property $ do
      -- Verifies that all logical type info is retained in the json encoding of the schema
      tripping schema'Logical A.toJSON A.fromJSON
