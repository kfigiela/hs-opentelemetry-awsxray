{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-concepts.html#xray-concepts-tracingheader>
--
-- Example:
--
-- @Root=1-5759e988-bd862e3fe1be46a994272793;Parent=53995c3f42cd8ad8;Sampled=1@
--
-- @Root=1-{epoch}-{unique}[;Parent={spanId}][;Sampled={1|0}][;meta=attr...]@
--
module OpenTelemetry.AWSXRay.TraceInfo
  ( TraceInfo(..)
  , fromXRayHeader
  , toXRayHeader
  , traceIdFromXRayHeader
  ) where

import Prelude

import Control.Error.Util (note, hush)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified OpenTelemetry.AWSXRay.Baggage as Baggage
import OpenTelemetry.Baggage (Baggage)
import OpenTelemetry.Trace (SpanContext(..))
import OpenTelemetry.Trace.Core
  (defaultTraceFlags, isSampled, setSampled, unsetSampled)
import OpenTelemetry.Trace.Id
  ( Base(..)
  , baseEncodedToSpanId
  , baseEncodedToTraceId
  , spanIdBaseEncodedByteString
  , traceIdBaseEncodedByteString, TraceId
  )
import qualified OpenTelemetry.Trace.TraceState as TS
import Control.Monad ((>=>))

-- | The data to read/write from the @X-Amzn-TraceId@ header
data TraceInfo = TraceInfo
  { spanContext :: SpanContext
  , baggage :: Maybe Baggage
  }
  deriving stock Show

traceIdFromXRayHeader :: ByteString -> Maybe TraceId
traceIdFromXRayHeader = hush . bsToKeyValues >=> rootTraceId

rootTraceId :: [(ByteString, ByteString)] -> Maybe TraceId
rootTraceId kv = do
  root <- lookup "Root" kv
  ["1", epoch, unique] <- pure $ bsSplitOn '-' root
  let
    -- AWS may trim leading zeros from epoch; we must put them back for it
    -- to be valid for OTel
    epochUnique = bsLeftPad 8 '0' epoch <> unique
  hush $ baseEncodedToTraceId Base16 epochUnique

fromXRayHeader :: ByteString -> Either String TraceInfo
fromXRayHeader bs = do
  kv <- bsToKeyValues bs
  traceId <- note "Root invalid or not present" $ rootTraceId kv
  parent <- note "Parent not present" $ lookup "Parent" kv
  spanId <- prefix "Parent is not a valid SpanId"
    $ baseEncodedToSpanId Base16 parent

  let
    traceFlags = case (== "1") <$> lookup "Sampled" kv of
      Nothing -> defaultTraceFlags
      Just True -> setSampled defaultTraceFlags
      Just False -> unsetSampled defaultTraceFlags

    baggage = Baggage.decode kv

  pure $ TraceInfo
    { spanContext = SpanContext
      { traceFlags
      , traceId
      , spanId
      , isRemote = True
      , traceState = TS.empty
      }
    , baggage
    }

toXRayHeader :: TraceInfo -> ByteString
toXRayHeader TraceInfo { spanContext, baggage } =
  bsFromKeyValues
    $ [ ("Root", "1-" <> epoch <> "-" <> unique)
      , ("Parent", spanIdBaseEncodedByteString Base16 spanId)
      , ("Sampled", if isSampled traceFlags then "1" else "0")
      ]
    <> maybe [] Baggage.encode baggage
 where
  SpanContext { traceId, spanId, traceFlags } = spanContext
  (epoch, unique) = BS.splitAt 8 $ traceIdBaseEncodedByteString Base16 traceId

prefix :: String -> Either String a -> Either String a
prefix p = first (\e -> p <> ": " <> e)

bsToKeyValues :: ByteString -> Either String [(ByteString, ByteString)]
bsToKeyValues = traverse go . bsSplitOn ';'
 where
  go bs = case bsSplitOn '=' bs of
    k : vs -> Right (k, mconcat vs)
    _ -> Left "No = found in key-value piece"

bsFromKeyValues :: [(ByteString, ByteString)] -> ByteString
bsFromKeyValues = BS.intercalate ";" . map (\(k, v) -> k <> "=" <> v)

bsLeftPad :: Int -> Char -> ByteString -> ByteString
bsLeftPad n c bs
  | diff > 0 = BS8.replicate diff c <> bs
  | otherwise = bs
  where diff = BS.length bs - n

bsSplitOn :: Char -> ByteString -> [ByteString]
bsSplitOn c = BS8.splitWith (== c)
