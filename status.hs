{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (lines, unwords, words, readFile)
import Text.Printf
import Data.Text as T hiding (filter, any, map, head, tail, last)
import Data.Text.Read
import Data.Text.IO
import Data.Time
import Network.MPD
import System.Locale
import System.IO (hClose, hSetBuffering, stdout, BufferMode(..))
import System.Process
import System.Posix hiding (append)
import Control.Concurrent
default (T.Text)

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
printPercent percent = colorize percent (pack $ printf "%0.2f%%" percent)

printPercent' :: Double -> Text
printPercent' percent = colorize (percent - 100) (pack $ printf "%0.2f%%" percent)

extractDouble :: Text -> Double
extractDouble ts = unWrap . rational $ strip ts
                 where
                  unWrap (Right x) = fst x
                  unWrap (Left _) = 0

displayBattery :: Text -> Text -> Text
displayBattery nowIO fullIO = "BAT: " `append` printPercent' (now / full * 100)
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

displaySound :: Text -> Text
displaySound soundIO = "Sound: " `append` percent
                     where
                       output = words . last $ lines soundIO
                       (_:_:_:percent:_) = output

displayMPD :: Response State -> Response (Maybe Song) -> Text
displayMPD (Left state) _ = "MPD: MIA"
displayMPD (Right state) (Right (Nothing)) = "MPD: ||"
displayMPD (Right state) (Right (Just song)) = "MPD: " `append` title `append` " By " `append` artist
                where
                  (Just titleValues) = sgGetTag Title song
                  (Just artistValues) = sgGetTag Artist song
                  title = toText . head $ titleValues
                  artist = toText . head $ artistValues

wrapFg :: Text -> Text -> Text
wrapFg color text = "^fg(" `append` color `append` ")" `append` text `append` "^fg()"

colorize :: Double -> Text -> Text
colorize percent text
  | percent <= 5   = wrapFg "#009900" text
  | percent <= 10  = wrapFg "#119900" text
  | percent <= 15  = wrapFg "#229900" text
  | percent <= 20  = wrapFg "#339900" text
  | percent <= 25  = wrapFg "#449900" text
  | percent <= 30  = wrapFg "#559900" text
  | percent <= 35  = wrapFg "#669900" text
  | percent <= 40  = wrapFg "#779900" text
  | percent <= 45  = wrapFg "#889900" text
  | percent <= 50  = wrapFg "#999900" text
  | percent <= 55  = wrapFg "#998800" text
  | percent <= 60  = wrapFg "#997700" text
  | percent <= 65  = wrapFg "#996600" text
  | percent <= 70  = wrapFg "#995500" text
  | percent <= 75  = wrapFg "#994400" text
  | percent <= 80  = wrapFg "#993300" text
  | percent <= 85  = wrapFg "#992200" text
  | percent <= 90  = wrapFg "#991100" text
  | percent <= 100 = wrapFg "#990000" text

readFile' :: Text -> IO Text
readFile' t = readFile $ unpack t



-- TODO: MPD, dzen
mainL :: Double -> Double -> IO ()
mainL w t = do
  nowIO   <- readFile' chargeNowPath
  fullIO  <- readFile' chargeFullPath
  memIO   <- readFile' memInfoPath
  timeIO  <- getZonedTime
  statIO  <- readFile' "/proc/stat"
  (inH,outH,errH,pid) <- runInteractiveProcess "amixer" ["-c", "0", "get", "Master"] Nothing Nothing
  hClose inH
  soundIO <- hGetContents outH
  state <- withMPD $ status >>= (\x -> return(stState x))
  song <- withMPD $ currentSong

  let timeText    = "DATE: " `append` (pack $ formatTime defaultTimeLocale "%A %b %d %r" timeIO)
  let batteryText = displayBattery nowIO fullIO
  let memText     = displayMemInfo $ listToMemInfo $ filterMemInfo memIO
  let (cpuText, work, total)     = displayCpuInfo (statIO, w, t)
  let soundText = displaySound soundIO
  let mpdText = displayMPD state song

  hPutStrLn stdout $ unwords [ timeText
                 , cpuText
                 , memText
                 , batteryText
                 , mpdText
                 , soundText
                 ]
  threadDelay 1000000
  mainL work total

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainL 0 0
