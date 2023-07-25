-- |
--
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-sendingdata.html#xray-api-traceids>
--
module OpenTelemetry.AWSXRay.IdGenerator
  ( awsXRayIdGenerator
  ) where

import Prelude

import Data.Binary.Builder (putWord32be, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.IO (unsafePerformIO)
import OpenTelemetry.Trace.Id.Generator ( IdGenerator(..) )
import OpenTelemetry.Trace.Id.Generator.Default (defaultIdGenerator)
import System.Random.MWC (createSystemRandom)
import System.Random.Stateful (uniformByteStringM)
import Data.Word (Word32)

awsXRayIdGenerator :: IdGenerator
{-# NOINLINE awsXRayIdGenerator #-}
awsXRayIdGenerator = unsafePerformIO $ do
  g <- createSystemRandom
  pure
    IdGenerator
      { generateSpanIdBytes = generateSpanIdBytes defaultIdGenerator
      , generateTraceIdBytes = do
          epoch <- round @_ @Word32 <$> getPOSIXTime
          unique <- uniformByteStringM 12 g
          pure $ toStrict (toLazyByteString $ putWord32be epoch) <> unique
      }
