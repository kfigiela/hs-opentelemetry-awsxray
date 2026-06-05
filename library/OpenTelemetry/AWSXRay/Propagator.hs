module OpenTelemetry.AWSXRay.Propagator
  ( awsXRayContextPropagator
  , awsXRayContextPropagatorOnError
  , FromHeaderMode(..)
  ) where

import Prelude

import Control.Error.Util (note)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, ResponseHeaders)
import OpenTelemetry.AWSXRay.TraceInfo
import OpenTelemetry.Context (Context, insertBaggage, insertSpan, lookupBaggage, lookupSpan)
import OpenTelemetry.Propagator
import OpenTelemetry.Trace.Core (getSpanContext, wrapSpanContext)
import OpenTelemetry.Context (insertExternalTraceId)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)


data FromHeaderMode =
    -- | Require Parent span id, assume unsampled if not sampled
    XRay
    -- | AWS ALB compatibility mode, only sets trace id without referring to parent span
    | ALB

awsXRayContextPropagator :: FromHeaderMode -> TextMapPropagator
awsXRayContextPropagator mode = awsXRayContextPropagatorOnError mode $ \_ _ -> pure ()

awsXRayContextPropagatorOnError
  :: FromHeaderMode
  -> (TextMap -> String -> IO ())
  -- ^ Called on failure to find or parse an @X-Amzn-Trace-Id@ header
  -> TextMapPropagator
awsXRayContextPropagatorOnError mode onErr = Propagator
  { propagatorFields = [hAmznTraceId]
  , extractor = \hs c ->
    case mode of
      ALB -> do
        let tid = textMapLookup hAmznTraceId hs >>= traceIdFromXRayHeader . encodeUtf8
        pure $ maybe id insertExternalTraceId tid $ c
      XRay -> do
        case fromXRayHeader . encodeUtf8 =<< note "not found" (textMapLookup hAmznTraceId hs) of
          Left err -> c <$ onErr hs err
          Right TraceInfo {..} -> do
            let wrapped = wrapSpanContext spanContext
            pure $ maybe id insertBaggage baggage $ insertSpan wrapped c
  , injector = \c hs -> case lookupSpan c of
    Nothing -> pure hs
    Just sp -> do
      info <- TraceInfo <$> getSpanContext sp <*> pure (lookupBaggage c)
      pure $ textMapInsert hAmznTraceId (decodeUtf8 $ toXRayHeader info) hs
  }

hAmznTraceId :: Text
hAmznTraceId = "x-amzn-trace-id"
