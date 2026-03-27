module Avro.Gen.Schema
where

import Data.Avro.Schema.Schema

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import           Hedgehog.Range (Range)
import qualified Hedgehog.Range as Range

null :: MonadGen m => m Schema
null = pure Null

boolean :: MonadGen m => m Schema
boolean = pure Boolean

decimalGen :: MonadGen m => m Decimal
decimalGen = Decimal
  <$> Gen.integral (Range.linear 0 10)
  <*> Gen.integral (Range.linear 0 10)

int :: MonadGen m => m Schema
int =
  fmap Int $ logicalTypeGen $ Gen.choice [
    DecimalI <$> decimalGen,
    pure Date,
    pure TimeMillis
  ]

long :: MonadGen m => m Schema
long =
  fmap Long $ logicalTypeGen $ Gen.choice [
    DecimalL <$> decimalGen,
    pure TimeMicros,
    pure TimestampMillis,
    pure TimestampMicros,
    pure LocalTimestampMillis,
    pure LocalTimestampMicros
  ]


logicalTypeGen :: MonadGen m => m a -> m (LogicalType a)
logicalTypeGen genKnownLogicalType =
  Gen.sized $ \n ->
    Gen.frequency [
        (2, pure NoLogicalType)
      , (1, UnknownLogicalType <$> Gen.text (Range.linear 0 20) Gen.alphaNum)
      , (1 + fromIntegral n, KnownLogicalType <$> genKnownLogicalType)
      ]
