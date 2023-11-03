module OpenTelemetry.AWSXRay.Sampler
  ( awsXRayTraceIdRatioBased
  )
  where

import Prelude

import Data.Binary.Get
import qualified Data.ByteString  as B
import qualified Data.ByteString.Lazy  as L
import Data.Text
import OpenTelemetry.Attributes (ToAttribute (toAttribute))
import OpenTelemetry.Context
import OpenTelemetry.Trace.Core (getSpanContext, traceState)
import OpenTelemetry.Trace.Id
import OpenTelemetry.Trace.Sampler
import OpenTelemetry.Trace.TraceState as TraceState
import Data.Word (Word64)
import qualified Data.HashMap.Strict as H

-- | identical as traceIdRatioBased except it drops first 4 bytes as they indicate timestamp in XRay
awsXRayTraceIdRatioBased :: Double -> Sampler
awsXRayTraceIdRatioBased fraction | fraction >= 1 = alwaysOn
awsXRayTraceIdRatioBased fraction | fraction <= 0 = alwaysOff
awsXRayTraceIdRatioBased fraction = sampler
  where
    sampleRate =
      if fraction > 0
        then toAttribute (round (1 / fraction) :: Int)
        else toAttribute (0 :: Int)

    traceIdUpperBound = floor (min maxWord (fraction * maxWord)) :: Word64
    maxWord = fromIntegral $ maxBound @Word64
    sampler =
      Sampler
        { getDescription = "TraceIdRatioBasedXRay{" <> pack (show fraction) <> "}"
        , shouldSample = \ctxt tid _ _ -> do
            mspanCtxt <- sequence (getSpanContext <$> lookupSpan ctxt)
            let x = runGet getWord64be (L.fromStrict $ B.take 8 $ B.drop 4 $ traceIdBytes tid)
            if x < traceIdUpperBound
              then do
                pure (RecordAndSample, H.fromList [("sampleRate", sampleRate)], maybe TraceState.empty traceState mspanCtxt)
              else pure (Drop, mempty, maybe TraceState.empty traceState mspanCtxt)
        }
