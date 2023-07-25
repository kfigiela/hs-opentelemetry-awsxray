{-# LANGUAGE NumericUnderscores #-}
module OpenTelemetry.AWSXRay.SamplerSpec
  ( spec
  ) where

import Prelude

import OpenTelemetry.Trace.Core (defaultSpanArguments)
import Test.Hspec
import OpenTelemetry.Trace.Sampler (SamplingResult(..), Sampler (..))
import OpenTelemetry.Context (empty)
import OpenTelemetry.AWSXRay.Sampler (awsXRayTraceIdRatioBased)
import OpenTelemetry.AWSXRay.IdGenerator (awsXRayIdGenerator)
import OpenTelemetry.Internal.Trace.Id (newTraceId)
import Control.Monad (forM_, foldM)

spec :: Spec
spec = do
  describe "Sampler" $ forM_ [0.0, 0.1, 0.2, 0.5, 0.8, 0.9, 1.0] $ \prob -> do
    let numSamples = 100_000
    it ("is will sample with correct distribution " <> show prob) $ do
      let sample tid = shouldSample (awsXRayTraceIdRatioBased prob) empty tid mempty defaultSpanArguments
          trySample acc _ = do
            newTraceId awsXRayIdGenerator >>= sample >>= pure . \case
              (RecordAndSample, _, _) -> acc + 1
              _ -> acc

      success <- foldM trySample 0 [0 :: Int .. numSamples]
      let expected = numSamples * prob
          absDifference = abs (success - expected)
          tolerance = numSamples * 0.02
      absDifference `shouldSatisfy` (<= tolerance)
