{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (lines, unwords, words)
import Shelly
import Text.Printf
import Data.Text.Lazy as LT hiding (filter, any, map, head, tail)
import Data.Text.Lazy.Read
import Data.Time
import System.Locale
import System.IO
import System.Posix hiding (append)
default (LT.Text)

batteryPath :: Text
batteryPath = "/sys/class/power_supply/BAT0/"
chargeNowPath :: Text
chargeNowPath = batteryPath `append` "charge_now"
chargeFullPath :: Text
chargeFullPath = batteryPath `append` "charge_full"
memInfoPath :: Text
memInfoPath = "/proc/meminfo"

data MemInfo = MemInfo {
    mTotal :: Double
  , mFree  :: Double
  , mBuffers  :: Double
  , mCached   :: Double
}deriving(Show)

printPercent :: Double -> Text
printPercent percent = pack $ printf "%0.2f%%" percent

extractDouble :: Text -> Double
extractDouble ts = unWrap . rational $ strip ts
                 where
                  unWrap (Right x) = fst x
                  unWrap (Left _) = 0

displayBattery :: Text -> Text -> Text
displayBattery nowIO fullIO = "BAT: " `append` printPercent (now / full * 100)
                            where
                              now  = extractDouble nowIO
                              full = extractDouble fullIO

filterMemInfo :: Text -> [Text]
filterMemInfo memIO = filter isMemInfo ms
                    where
                      ms = lines memIO
                      memStrings = ["MemTotal","MemFree","Buffers","Cached"]
                      isMemInfo x = any (\y -> y x ) $ map isPrefixOf memStrings

listToMemInfo :: [Text] -> MemInfo
listToMemInfo texts = MemInfo total free buffers cached
                    where
                      (total:free:buffers:cached:[]) = textsToDoubles texts

textsToDoubles :: [Text] -> [Double]
textsToDoubles [] = []
textsToDoubles (x : xs) = parseMem x : textsToDoubles xs
                      where
                        parseMem y = extractDouble . snd $ breakOnEnd ":" y

displayMemInfo :: MemInfo -> Text
displayMemInfo (MemInfo total free buffers cached) = display
                       where
                        used = total - (free + buffers + cached)
                        percent = used / total * 100
                        display = "MEM: " `append` printPercent percent

displayCpuInfo :: (Text, Double, Double) -> (Text, Double, Double)
displayCpuInfo (procText, prevWork, prevTotal) = ("CPU: " `append` display, workCycles, totalCycles)
                        where
                          cpu = map extractDouble . tail . words . head $ lines procText
                          (user:nice:system:_) = cpu
                          workCycles = user + nice + system
                          totalCycles = sum cpu
                          deltaWork = workCycles - prevWork
                          deltaTotal = totalCycles - prevTotal
                          display = printPercent (deltaWork / deltaTotal * 100)


-- TODO: Sound, MPD, Color, dzen
mainL :: Double -> Double -> IO ()
mainL w t = shelly $ do
  nowIO  <- readfile $ fromText chargeNowPath
  fullIO <- readfile $ fromText chargeFullPath
  memIO  <- readfile $ fromText memInfoPath
  timeIO <- liftIO getZonedTime
  statIO <- readfile $ fromText "/proc/stat"

  let timeText    = (pack "DATE: ") `append` (pack $ formatTime defaultTimeLocale "%A %b %d %r" timeIO)
  let batteryText = displayBattery nowIO fullIO
  let memText     = displayMemInfo $ listToMemInfo $ filterMemInfo memIO
  let (cpuText, work, total)     = displayCpuInfo (statIO, w, t)

  echo $ unwords [timeText, cpuText, batteryText, memText]
  liftIO $ nanosleep 500000000
  liftIO $ mainL work total

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainL 0 0
