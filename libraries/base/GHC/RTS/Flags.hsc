{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Accessors to GHC RTS flags.
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime-control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
-- @since 4.8.0.0
--
module GHC.RTS.Flags
  ( RtsTime
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , ParFlags (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  , getParFlags
  ) where

#include "Rts.h"
#include "rts/Flags.h"

import Control.Applicative
import Control.Monad

import Foreign
import Foreign.C

import GHC.Base
import GHC.Enum
import GHC.IO
import GHC.Real
import GHC.Show

-- | @'Time'@ is defined as a @'StgWord64'@ in @stg/Types.h@
--
-- @since 4.8.2.0
type RtsTime = Word64

-- | Should we produce a summary of the garbage collector statistics after the
-- program has exited?
--
-- @since 4.8.2.0
data GiveGCStats
    = NoGCStats
    | CollectGCStats
    | OneLineGCStats
    | SummaryGCStats
    | VerboseGCStats
    deriving (Show)

-- | @since 4.8.0.0
instance Enum GiveGCStats where
    fromEnum NoGCStats      = #{const NO_GC_STATS}
    fromEnum CollectGCStats = #{const COLLECT_GC_STATS}
    fromEnum OneLineGCStats = #{const ONELINE_GC_STATS}
    fromEnum SummaryGCStats = #{const SUMMARY_GC_STATS}
    fromEnum VerboseGCStats = #{const VERBOSE_GC_STATS}

    toEnum #{const NO_GC_STATS}      = NoGCStats
    toEnum #{const COLLECT_GC_STATS} = CollectGCStats
    toEnum #{const ONELINE_GC_STATS} = OneLineGCStats
    toEnum #{const SUMMARY_GC_STATS} = SummaryGCStats
    toEnum #{const VERBOSE_GC_STATS} = VerboseGCStats
    toEnum e = errorWithoutStackTrace ("invalid enum for GiveGCStats: " ++ show e)

-- | Parameters of the garbage collector.
--
-- @since 4.8.0.0
data GCFlags = GCFlags
    { statsFile             :: Maybe FilePath
    , giveStats             :: GiveGCStats
    , maxStkSize            :: Word32
    , initialStkSize        :: Word32
    , stkChunkSize          :: Word32
    , stkChunkBufferSize    :: Word32
    , maxHeapSize           :: Word32
    , minAllocAreaSize      :: Word32
    , largeAllocLim         :: Word32
    , nurseryChunkSize      :: Word32
    , minOldGenSize         :: Word32
    , heapSizeSuggestion    :: Word32
    , heapSizeSuggestionAuto :: Bool
    , oldGenFactor          :: Double
    , pcFreeHeap            :: Double
    , generations           :: Word32
    , squeezeUpdFrames      :: Bool
    , compact               :: Bool -- ^ True <=> "compact all the time"
    , compactThreshold      :: Double
    , sweep                 :: Bool
      -- ^ use "mostly mark-sweep" instead of copying for the oldest generation
    , ringBell              :: Bool
    , idleGCDelayTime       :: RtsTime
    , doIdleGC              :: Bool
    , heapBase              :: Word -- ^ address to ask the OS for memory
    , allocLimitGrace       :: Word
    , numa                  :: Bool
    , nNumaNodes            :: Word32
    } deriving (Show)

-- | Parameters concerning context switching
--
-- @since 4.8.0.0
data ConcFlags = ConcFlags
    { ctxtSwitchTime  :: RtsTime
    , ctxtSwitchTicks :: Int
    } deriving (Show)

-- | Miscellaneous parameters
--
-- @since 4.8.0.0
data MiscFlags = MiscFlags
    { tickInterval          :: RtsTime
    , installSignalHandlers :: Bool
    , machineReadable       :: Bool
    , linkerMemBase         :: Word
      -- ^ address to ask the OS for memory for the linker, 0 ==> off
    } deriving (Show)

-- | Flags to control debugging output & extra checking in various
-- subsystems.
--
-- @since 4.8.0.0
data DebugFlags = DebugFlags
    { scheduler   :: Bool -- ^ 's'
    , interpreter :: Bool -- ^ 'i'
    , weak        :: Bool -- ^ 'w'
    , gccafs      :: Bool -- ^ 'G'
    , gc          :: Bool -- ^ 'g'
    , block_alloc :: Bool -- ^ 'b'
    , sanity      :: Bool -- ^ 'S'
    , stable      :: Bool -- ^ 't'
    , prof        :: Bool -- ^ 'p'
    , linker      :: Bool -- ^ 'l' the object linker
    , apply       :: Bool -- ^ 'a'
    , stm         :: Bool -- ^ 'm'
    , squeeze     :: Bool -- ^ 'z' stack squeezing & lazy blackholing
    , hpc         :: Bool -- ^ 'c' coverage
    , sparks      :: Bool -- ^ 'r'
    } deriving (Show)

-- | Should the RTS produce a cost-center summary?
--
-- @since 4.8.2.0
data DoCostCentres
    = CostCentresNone
    | CostCentresSummary
    | CostCentresVerbose
    | CostCentresAll
    | CostCentresXML
    deriving (Show)

-- | @since 4.8.0.0
instance Enum DoCostCentres where
    fromEnum CostCentresNone    = #{const COST_CENTRES_NONE}
    fromEnum CostCentresSummary = #{const COST_CENTRES_SUMMARY}
    fromEnum CostCentresVerbose = #{const COST_CENTRES_VERBOSE}
    fromEnum CostCentresAll     = #{const COST_CENTRES_ALL}
    fromEnum CostCentresXML     = #{const COST_CENTRES_XML}

    toEnum #{const COST_CENTRES_NONE}    = CostCentresNone
    toEnum #{const COST_CENTRES_SUMMARY} = CostCentresSummary
    toEnum #{const COST_CENTRES_VERBOSE} = CostCentresVerbose
    toEnum #{const COST_CENTRES_ALL}     = CostCentresAll
    toEnum #{const COST_CENTRES_XML}     = CostCentresXML
    toEnum e = errorWithoutStackTrace ("invalid enum for DoCostCentres: " ++ show e)

-- | Parameters pertaining to the cost-center profiler.
--
-- @since 4.8.0.0
data CCFlags = CCFlags
    { doCostCentres :: DoCostCentres
    , profilerTicks :: Int
    , msecsPerTick  :: Int
    } deriving (Show)

-- | What sort of heap profile are we collecting?
--
-- @since 4.8.2.0
data DoHeapProfile
    = NoHeapProfiling
    | HeapByCCS
    | HeapByMod
    | HeapByDescr
    | HeapByType
    | HeapByRetainer
    | HeapByLDV
    | HeapByClosureType
    deriving (Show)

-- | @since 4.8.0.0
instance Enum DoHeapProfile where
    fromEnum NoHeapProfiling   = #{const NO_HEAP_PROFILING}
    fromEnum HeapByCCS         = #{const HEAP_BY_CCS}
    fromEnum HeapByMod         = #{const HEAP_BY_MOD}
    fromEnum HeapByDescr       = #{const HEAP_BY_DESCR}
    fromEnum HeapByType        = #{const HEAP_BY_TYPE}
    fromEnum HeapByRetainer    = #{const HEAP_BY_RETAINER}
    fromEnum HeapByLDV         = #{const HEAP_BY_LDV}
    fromEnum HeapByClosureType = #{const HEAP_BY_CLOSURE_TYPE}

    toEnum #{const NO_HEAP_PROFILING}    = NoHeapProfiling
    toEnum #{const HEAP_BY_CCS}          = HeapByCCS
    toEnum #{const HEAP_BY_MOD}          = HeapByMod
    toEnum #{const HEAP_BY_DESCR}        = HeapByDescr
    toEnum #{const HEAP_BY_TYPE}         = HeapByType
    toEnum #{const HEAP_BY_RETAINER}     = HeapByRetainer
    toEnum #{const HEAP_BY_LDV}          = HeapByLDV
    toEnum #{const HEAP_BY_CLOSURE_TYPE} = HeapByClosureType
    toEnum e = errorWithoutStackTrace ("invalid enum for DoHeapProfile: " ++ show e)

-- | Parameters of the cost-center profiler
--
-- @since 4.8.0.0
data ProfFlags = ProfFlags
    { doHeapProfile            :: DoHeapProfile
    , heapProfileInterval      :: RtsTime -- ^ time between samples
    , heapProfileIntervalTicks :: Word    -- ^ ticks between samples (derived)
    , includeTSOs              :: Bool
    , showCCSOnException       :: Bool
    , maxRetainerSetSize       :: Word
    , ccsLength                :: Word
    , modSelector              :: Maybe String
    , descrSelector            :: Maybe String
    , typeSelector             :: Maybe String
    , ccSelector               :: Maybe String
    , ccsSelector              :: Maybe String
    , retainerSelector         :: Maybe String
    , bioSelector              :: Maybe String
    } deriving (Show)

-- | Is event tracing enabled?
--
-- @since 4.8.2.0
data DoTrace
    = TraceNone      -- ^ no tracing
    | TraceEventLog  -- ^ send tracing events to the event log
    | TraceStderr    -- ^ send tracing events to @stderr@
    deriving (Show)

-- | @since 4.8.0.0
instance Enum DoTrace where
    fromEnum TraceNone     = #{const TRACE_NONE}
    fromEnum TraceEventLog = #{const TRACE_EVENTLOG}
    fromEnum TraceStderr   = #{const TRACE_STDERR}

    toEnum #{const TRACE_NONE}     = TraceNone
    toEnum #{const TRACE_EVENTLOG} = TraceEventLog
    toEnum #{const TRACE_STDERR}   = TraceStderr
    toEnum e = errorWithoutStackTrace ("invalid enum for DoTrace: " ++ show e)

-- | Parameters pertaining to event tracing
--
-- @since 4.8.0.0
data TraceFlags = TraceFlags
    { tracing        :: DoTrace
    , timestamp      :: Bool -- ^ show timestamp in stderr output
    , traceScheduler :: Bool -- ^ trace scheduler events
    , traceGc        :: Bool -- ^ trace GC events
    , sparksSampled  :: Bool -- ^ trace spark events by a sampled method
    , sparksFull     :: Bool -- ^ trace spark events 100% accurately
    , user           :: Bool -- ^ trace user events (emitted from Haskell code)
    } deriving (Show)

-- | Parameters pertaining to ticky-ticky profiler
--
-- @since 4.8.0.0
data TickyFlags = TickyFlags
    { showTickyStats :: Bool
    , tickyFile      :: Maybe FilePath
    } deriving (Show)

-- | Parameters pertaining to parallelism
--
-- @since 4.8.0.0
data ParFlags = ParFlags
    { nCapabilities :: Word32
    , migrate :: Bool
    , maxLocalSparks :: Word32
    , parGcEnabled :: Bool
    , parGcGen :: Word32
    , parGcLoadBalancingEnabled :: Bool
    , parGcLoadBalancingGen :: Word32
    , parGcNoSyncWithIdle :: Word32
    , parGcThreads :: Word32
    , setAffinity :: Bool
    }
    deriving (Show)

-- | Parameters of the runtime system
--
-- @since 4.8.0.0
data RTSFlags = RTSFlags
    { gcFlags         :: GCFlags
    , concurrentFlags :: ConcFlags
    , miscFlags       :: MiscFlags
    , debugFlags      :: DebugFlags
    , costCentreFlags :: CCFlags
    , profilingFlags  :: ProfFlags
    , traceFlags      :: TraceFlags
    , tickyFlags      :: TickyFlags
    , parFlags        :: ParFlags
    } deriving (Show)

foreign import ccall "&RtsFlags" rtsFlagsPtr :: Ptr RTSFlags

getRTSFlags :: IO RTSFlags
getRTSFlags = do
  RTSFlags <$> getGCFlags
           <*> getConcFlags
           <*> getMiscFlags
           <*> getDebugFlags
           <*> getCCFlags
           <*> getProfFlags
           <*> getTraceFlags
           <*> getTickyFlags
           <*> getParFlags

peekFilePath :: Ptr () -> IO (Maybe FilePath)
peekFilePath ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = return (Just "<filepath>")

-- | Read a NUL terminated string. Return Nothing in case of a NULL pointer.
peekCStringOpt :: Ptr CChar -> IO (Maybe String)
peekCStringOpt ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekCString ptr

getGCFlags :: IO GCFlags
getGCFlags = do
  let ptr = (#ptr RTS_FLAGS, GcFlags) rtsFlagsPtr
  GCFlags <$> (peekFilePath =<< #{peek GC_FLAGS, statsFile} ptr)
          <*> (toEnum . fromIntegral <$>
                (#{peek GC_FLAGS, giveStats} ptr :: IO Word32))
          <*> #{peek GC_FLAGS, maxStkSize} ptr
          <*> #{peek GC_FLAGS, initialStkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkSize} ptr
          <*> #{peek GC_FLAGS, stkChunkBufferSize} ptr
          <*> #{peek GC_FLAGS, maxHeapSize} ptr
          <*> #{peek GC_FLAGS, minAllocAreaSize} ptr
          <*> #{peek GC_FLAGS, largeAllocLim} ptr
          <*> #{peek GC_FLAGS, nurseryChunkSize} ptr
          <*> #{peek GC_FLAGS, minOldGenSize} ptr
          <*> #{peek GC_FLAGS, heapSizeSuggestion} ptr
          <*> #{peek GC_FLAGS, heapSizeSuggestionAuto} ptr
          <*> #{peek GC_FLAGS, oldGenFactor} ptr
          <*> #{peek GC_FLAGS, pcFreeHeap} ptr
          <*> #{peek GC_FLAGS, generations} ptr
          <*> #{peek GC_FLAGS, squeezeUpdFrames} ptr
          <*> #{peek GC_FLAGS, compact} ptr
          <*> #{peek GC_FLAGS, compactThreshold} ptr
          <*> #{peek GC_FLAGS, sweep} ptr
          <*> #{peek GC_FLAGS, ringBell} ptr
          <*> #{peek GC_FLAGS, idleGCDelayTime} ptr
          <*> #{peek GC_FLAGS, doIdleGC} ptr
          <*> #{peek GC_FLAGS, heapBase} ptr
          <*> #{peek GC_FLAGS, allocLimitGrace} ptr
          <*> #{peek GC_FLAGS, numa} ptr
          <*> #{peek GC_FLAGS, nNumaNodes} ptr

getParFlags :: IO ParFlags
getParFlags = do
  let ptr = (#ptr RTS_FLAGS, ParFlags) rtsFlagsPtr
  ParFlags
    <$> #{peek PAR_FLAGS, nCapabilities} ptr
    <*> #{peek PAR_FLAGS, migrate} ptr
    <*> #{peek PAR_FLAGS, maxLocalSparks} ptr
    <*> #{peek PAR_FLAGS, parGcEnabled} ptr
    <*> #{peek PAR_FLAGS, parGcGen} ptr
    <*> #{peek PAR_FLAGS, parGcLoadBalancingEnabled} ptr
    <*> #{peek PAR_FLAGS, parGcLoadBalancingGen} ptr
    <*> #{peek PAR_FLAGS, parGcNoSyncWithIdle} ptr
    <*> #{peek PAR_FLAGS, parGcThreads} ptr
    <*> #{peek PAR_FLAGS, setAffinity} ptr

getConcFlags :: IO ConcFlags
getConcFlags = do
  let ptr = (#ptr RTS_FLAGS, ConcFlags) rtsFlagsPtr
  ConcFlags <$> #{peek CONCURRENT_FLAGS, ctxtSwitchTime} ptr
            <*> #{peek CONCURRENT_FLAGS, ctxtSwitchTicks} ptr

getMiscFlags :: IO MiscFlags
getMiscFlags = do
  let ptr = (#ptr RTS_FLAGS, MiscFlags) rtsFlagsPtr
  MiscFlags <$> #{peek MISC_FLAGS, tickInterval} ptr
            <*> #{peek MISC_FLAGS, install_signal_handlers} ptr
            <*> #{peek MISC_FLAGS, machineReadable} ptr
            <*> #{peek MISC_FLAGS, linkerMemBase} ptr

getDebugFlags :: IO DebugFlags
getDebugFlags = do
  let ptr = (#ptr RTS_FLAGS, DebugFlags) rtsFlagsPtr
  DebugFlags <$> #{peek DEBUG_FLAGS, scheduler} ptr
             <*> #{peek DEBUG_FLAGS, interpreter} ptr
             <*> #{peek DEBUG_FLAGS, weak} ptr
             <*> #{peek DEBUG_FLAGS, gccafs} ptr
             <*> #{peek DEBUG_FLAGS, gc} ptr
             <*> #{peek DEBUG_FLAGS, block_alloc} ptr
             <*> #{peek DEBUG_FLAGS, sanity} ptr
             <*> #{peek DEBUG_FLAGS, stable} ptr
             <*> #{peek DEBUG_FLAGS, prof} ptr
             <*> #{peek DEBUG_FLAGS, linker} ptr
             <*> #{peek DEBUG_FLAGS, apply} ptr
             <*> #{peek DEBUG_FLAGS, stm} ptr
             <*> #{peek DEBUG_FLAGS, squeeze} ptr
             <*> #{peek DEBUG_FLAGS, hpc} ptr
             <*> #{peek DEBUG_FLAGS, sparks} ptr

getCCFlags :: IO CCFlags
getCCFlags = do
  let ptr = (#ptr RTS_FLAGS, GcFlags) rtsFlagsPtr
  CCFlags <$> (toEnum . fromIntegral
                <$> (#{peek COST_CENTRE_FLAGS, doCostCentres} ptr :: IO Word32))
          <*> #{peek COST_CENTRE_FLAGS, profilerTicks} ptr
          <*> #{peek COST_CENTRE_FLAGS, msecsPerTick} ptr

getProfFlags :: IO ProfFlags
getProfFlags = do
  let ptr = (#ptr RTS_FLAGS, ProfFlags) rtsFlagsPtr
  ProfFlags <$> (toEnum <$> #{peek PROFILING_FLAGS, doHeapProfile} ptr)
            <*> #{peek PROFILING_FLAGS, heapProfileInterval} ptr
            <*> #{peek PROFILING_FLAGS, heapProfileIntervalTicks} ptr
            <*> #{peek PROFILING_FLAGS, includeTSOs} ptr
            <*> #{peek PROFILING_FLAGS, showCCSOnException} ptr
            <*> #{peek PROFILING_FLAGS, maxRetainerSetSize} ptr
            <*> #{peek PROFILING_FLAGS, ccsLength} ptr
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, modSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, descrSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, typeSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, ccsSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, retainerSelector} ptr)
            <*> (peekCStringOpt =<< #{peek PROFILING_FLAGS, bioSelector} ptr)

getTraceFlags :: IO TraceFlags
getTraceFlags = do
  let ptr = (#ptr RTS_FLAGS, TraceFlags) rtsFlagsPtr
  TraceFlags <$> (toEnum . fromIntegral
                   <$> (#{peek TRACE_FLAGS, tracing} ptr :: IO CInt))
             <*> #{peek TRACE_FLAGS, timestamp} ptr
             <*> #{peek TRACE_FLAGS, scheduler} ptr
             <*> #{peek TRACE_FLAGS, gc} ptr
             <*> #{peek TRACE_FLAGS, sparks_sampled} ptr
             <*> #{peek TRACE_FLAGS, sparks_full} ptr
             <*> #{peek TRACE_FLAGS, user} ptr

getTickyFlags :: IO TickyFlags
getTickyFlags = do
  let ptr = (#ptr RTS_FLAGS, TickyFlags) rtsFlagsPtr
  TickyFlags <$> #{peek TICKY_FLAGS, showTickyStats} ptr
             <*> (peekFilePath =<< #{peek TICKY_FLAGS, tickyFile} ptr)
