{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}

module Benchmark.TimeKill
 (
   timeAndKillThreads
 )
 where 

import Control.Exception
import Control.Monad
import Data.IORef
import Data.Int
import Data.List
import Data.List.Split  hiding (split)
import GHC.Exts (traceEvent)
--import Benchmarks.BinSearch
import System.Random
import System.CPUTime  (getCPUTime)
import System.CPUTime.Rdtsc
import Text.Printf
import GHC.Conc (forkIO, numCapabilities, threadDelay, killThread)
import Control.Concurrent.MVar


-- NOTE: when measuring very fine grained actions note that every
-- iteration reads and writes a counter variable.
timeAndKillThreads :: Int -> Int64 -> String -> IO () -> IO ()
timeAndKillThreads numthreads freq descr action =
  do 

-- Note, if the IORefs are allocated on the main thread the throughput
-- in the parallel case plummets.  Presumably this is due to false
-- sharing as they get bump-allocated.
#if 0
     counters <- forM [1..numthreads] (const$ newIORef (1::Int64)) 
     tids <- forM counters $ \counter -> 
 	       forkIO $ infloop counter
#else
     mv <- newEmptyMVar
     tids <- forM [1..numthreads] $ \_ -> 
 	        forkIO $ do r <- newIORef (1::Int64)
			    putMVar mv r 
			    infloop r 
     counters <- forM [1..numthreads] (const$ takeMVar mv)
#endif

     threadDelay (1000*1000) -- One second
     mapM_ killThread tids

     finals <- mapM readIORef counters
--     printf "Across %d threads got these throughputs: %s\n" numthreads (show finals)
     let mean :: Double = fromIntegral (foldl1 (+) finals) / fromIntegral numthreads
         cycles_per :: Double = fromIntegral freq / mean

     printResult (round mean :: Int64) descr cycles_per

 where 
   infloop !counter = 
     do action
	incr counter        
	infloop counter 

   incr !counter = 
     do -- modifyIORef counter (+1) -- Not strict enough!
	c <- readIORef counter
	let c' = c+1
	_ <- evaluate c'
	writeIORef counter c'     



printResult ::  Int64 -> String -> Double -> IO ()
printResult total msg cycles_per = 
     putStrLn$ "    "++ padleft 11 (commaint total) ++" per/second average  "++ padright 27 ("["++msg++"]") ++" ~ "
	       ++ fmt_num cycles_per ++" cycles"


-- Readable large integer printing:
commaint :: Integral a => a -> String
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)

padleft :: Int -> String -> String
padleft n str | length str >= n = str
padleft n str | otherwise       = take (n - length str) (repeat ' ') ++ str

padright :: Int -> String -> String
padright n str | length str >= n = str
padright n str | otherwise       = str ++ take (n - length str) (repeat ' ')


fmt_num :: (RealFrac a, PrintfArg a) => a -> String
fmt_num n = if n < 100 
	    then printf "%.2f" n
	    else commaint (round n :: Integer)

-- -- Measure clock frequency, spinning rather than sleeping to try to
-- -- stay on the same core.
-- measureFreq :: IO Int64
-- measureFreq = do 
--   let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
--   t1 <- rdtsc 
--   start <- getCPUTime
--   let loop !n !last = 
--        do t2 <- rdtsc 
-- 	  when (t2 < last) $
-- 	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
-- 	  cput <- getCPUTime		
-- 	  if (cput - start < second) 
-- 	   then loop (n+1) t2
-- 	   else return (n,t2)
--   (n,t2) <- loop 0 t1
--   putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (n::Int64)
--   when (t2 < t1) $ 
--     putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

--   return$ fromIntegral (t2 - t1)

