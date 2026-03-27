{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Avro.Data.Logical (
  Logical (..),
  schema'Logical,
  logicalGen,
) where

import Data.Avro.Internal.Time (microsToDiffTime, microsToLocalTime, microsToUTCTime, millisToDiffTime, millisToLocalTime, millisToUTCTime)

import Data.Avro.Deriving (deriveAvroFromByteString, r)
import qualified Data.UUID as UUID

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

deriveAvroFromByteString [r|
{
  "name": "Logical",
  "type": "record",
  "fields": [
    {
      "name": "noLogicalType1",
      "type": "string"
    },
    {
      "name": "noLogicalType2",
      "type": {"type": "string"}
    },
    {
      "name": "someUnknownLogicalType",
      "type":
        {
          "logicalType": "logical-address",
          "type": "string"
        }
    },
    {
      "name": "logicalTypeOnWrongType",
      "type":
        {
          "logicalType": "timestamp-millis",
          "type": "string"
        }
    },
    {
      "name": "tsMillis",
      "type":
        {
          "logicalType": "timestamp-millis",
          "type": "long"
        }
    },
    {
      "name": "tsMicros",
      "type":
        {
          "logicalType": "timestamp-micros",
          "type": "long"
        }
    },
    {
      "name": "timeMillis",
      "type":
        {
          "logicalType": "time-millis",
          "type": "int"
        }
    },
    {
      "name": "timeMicros",
      "type":
        {
          "logicalType": "time-micros",
          "type": "long"
        }
    },
    {
      "name": "localTimestampMillis",
      "type":
        {
          "logicalType": "local-timestamp-millis",
          "type": "long"
        }
     },
     {
      "name": "localTimestampMicros",
      "type":
        {
          "logicalType": "local-timestamp-micros",
          "type": "long"
        }
     }
  ]
}
|]

logicalGen :: MonadGen m => m Logical
logicalGen = Logical
  <$> Gen.text (Range.linear 0 30) Gen.alphaNum
  <*> Gen.text (Range.linear 0 30) Gen.alphaNum
  <*> Gen.text (Range.linear 0 30) Gen.alphaNum
  <*> Gen.text (Range.linear 0 30) Gen.alphaNum
  <*> (millisToUTCTime  . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (microsToUTCTime  . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (millisToDiffTime . toInteger <$> Gen.int32 (Range.linear 0 maxBound))
  <*> (microsToDiffTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (millisToLocalTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
  <*> (microsToLocalTime . toInteger <$> Gen.int64 (Range.linear 0 maxBound))
