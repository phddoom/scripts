{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (lines, unwords, words, readFile, putStrLn)
import Text.Printf
import Data.Text as T hiding (filter, any, map, head, tail, last, take, foldl1')
import Data.List (foldl1')
import Data.Text.Read
import Data.Text.IO hiding (writeFile)
import Data.Time
import Network.MPD
import System.Locale
import System.IO (hSetBuffering, BufferMode(..), Handle)
import System.Process
import System.Posix.Process
import System.Posix.Signals
import Control.Concurrent
import Control.Monad (liftM)
import Control.Exception
import Numeric
default (T.Text)

batteryPath :: Text
batteryPath = "/sys/class/power_supply/BAT0/"
chargeNowPath :: Text
chargeNowPath = batteryPath `append` "charge_now"
chargeFullPath :: Text
chargeFullPath = batteryPath `append` "charge_full"
memInfoPath :: Text
memInfoPath = "/proc/meminfo"

data MemInfo = MemInfo Double Double Double Double

printPercent :: Double -> Text
printPercent percent = colorize percent (pack $ printf "%0.2f%%" percent)

printPercent' :: Double -> Text
printPercent' percent = colorize (100 - percent) (pack $ printf "%0.2f%%" percent)

extractDouble :: Text -> Double
extractDouble ts = unWrap . rational $ strip ts
  where
    unWrap (Right x) = fst x
    unWrap (Left _) = 0

displayBattery :: Text -> Text -> Text
displayBattery nowIO fullIO = if isNaN percent then "" else display
  where
    now  = extractDouble nowIO
    full = extractDouble fullIO
    percent = now / full * 100
    display = "BAT: " `append` printPercent' percent

filterMemInfo :: Text -> [Text]
filterMemInfo memIO = filter isMemInfo ms
  where
    ms          = lines memIO
    memStrings  = ["MemTotal","MemFree","Buffers","Cached"]
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
  used    = total - (free + buffers + cached)
  percent = used / total * 100
  display = "MEM: " `append` printPercent percent

displayCpuInfo :: (Text, Double, Double) -> (Text, Double, Double)
displayCpuInfo (procT, pWork, pTotal) = (display, workCycles, totalCycles)
  where
    cpu         = map extractDouble . tail . words . head $ lines procT
    workCycles  = sum $ take 3 cpu
    totalCycles = sum cpu
    dWork       = workCycles - pWork
    dTotal      = totalCycles - pTotal
    display     = "CPU: " `append` printPercent (dWork / dTotal * 100)

displaySound :: Text -> Text
displaySound soundIO = "Sound: " `append` display
  where
    output       = words . last $ lines soundIO
    isOn         = isInfixOf "[on]" $ last output
    percent      = output !! 3
    colorPercent = colorizeBool isOn percent
    display      = wrapAction "1" "amixer -c 0 set Master toggle" colorPercent

displayMPD :: Response State -> Response (Maybe Song) -> Text
displayMPD (Left _) _                    = unwords ["MPD:", wrapFg "red" "MIA"]
displayMPD (Right _) (Left _)            = unwords ["MPD:", wrapFg "red" "MIA"]
displayMPD (Right _) (Right Nothing)     = unwords ["MPD:", wrapFg "yellow" "||"]
displayMPD (Right _) (Right (Just song)) = displayPlay
  where
    (Just titleValues)  = sgGetTag Title song
    (Just artistValues) = sgGetTag Artist song
    title               = toText . head $ titleValues
    artist              = toText . head $ artistValues
    displayPlay         = unwords ["MPD:", title, "By", artist]

wrapFg :: Text -> Text -> Text
wrapFg color text = "^fg(" `append` color `append` ")" `append` text `append` "^fg()"

wrapAction :: Text -> Text -> Text -> Text
wrapAction button command text = foldl1' append texts
  where
    texts = ["^ca(", button, ",", command, ")", text, "^ca()"]

colorizeBool :: Bool -> Text -> Text
colorizeBool True text = wrapFg "green" text
colorizeBool False text = wrapFg "red" text

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
colorize _ text = text

chromo :: Double -> Text
chromo mins
    | mins <= 239 = "#00FF" `append` showHex' ((60 - mins) / 4)
    | mins == 240 = "#00FF00"
    | mins <= 479 = "#" `append` showHex' (mins - 240 / 4) `append` "FF00"
    | mins == 480 = "#FFFF00"
    | mins <= 719 = "#FF" `append` showHex' (60 - ((mins - 480) / 4)) `append` "00"
    | mins == 720 = "#FF0000"
    | mins <= 959 = "#FF00" `append` showHex' ((mins - 720) / 4)
    | mins == 960 = "#FF00FF"
    | mins <= 1199 = "#" `append` showHex' ((60 - (mins - 960)) / 4) `append` "00FF"
    | mins == 1200 = "#0000FF"
    | mins <= 1439 = "#00" `append` showHex' ((mins - 1200) / 4) `append` "FF"
    | otherwise  = "#FFFFFF"
  where
    showHex' x = pack . paddedHex $ hex x
    hex x = showHex (abs (round $ x * 4.25)::Integer) ""
    paddedHex hx | Prelude.length hx < 2 = "0" ++ hx
                 | Prelude.length hx > 2 = take 2 hx
                 | otherwise =  hx

displayTime :: ZonedTime -> Text
displayTime time = display
  where
    formatedTime = pack (formatTime defaultTimeLocale "%A %b %d %r" time)
    tod = localTimeOfDay $ zonedTimeToLocalTime time
    hour = todHour tod
    mins = todMin  tod
    ticks = fromIntegral ((hour * 60) + mins) :: Double
    colorTime = wrapFg (chromo ticks) formatedTime
    display = "DATE: " `append` colorTime

readFile' :: Text -> IO Text
readFile' t = handle (\(_ :: IOException) -> return "") $ readFile $ unpack t

readProcess' :: Text -> [Text] -> Text -> IO Text
readProcess' command args input = liftM pack tReadProcess
  where
    command' = unpack command
    args' = map unpack args
    input' = unpack input
    tReadProcess = readProcess command' args' input'


-- TODO: debug output
mainL :: Handle -> Double -> Double -> IO ()
mainL pipe w t = do
  nowIO   <- readFile' chargeNowPath
  fullIO  <- readFile' chargeFullPath
  memIO   <- readFile' memInfoPath
  timeIO  <- getZonedTime
  statIO  <- readFile' "/proc/stat"
  soundIO <- readProcess' "amixer" ["-c", "0", "get", "Master"] empty
  state <- withMPD $ liftM stState status
  song <- withMPD currentSong

  let timeText    = displayTime timeIO
  let batteryText = displayBattery nowIO fullIO
  let memText     = displayMemInfo $ listToMemInfo $ filterMemInfo memIO
  let (cpuText, work, total)     = displayCpuInfo (statIO, w, t)
  let soundText = displaySound soundIO
  let mpdText = displayMPD state song

  hPutStrLn pipe $ unwords [ timeText
                 , cpuText
                 , memText
                 , batteryText
                 , mpdText
                 , soundText
                 ]

  threadDelay 1000000
  mainL pipe work total

killHandler :: ProcessHandle -> IO ()
killHandler pid = terminateProcess pid

main :: IO ()
main = do
  let dzenCmd = "dzen2 -p -y -1 -fn Terminus-14 -ta l -e onstart=lower -xs 1"
  let dzen = (shell dzenCmd){ std_in = CreatePipe }
  (Just pipe, _, _, dPid) <- createProcess dzen
  hSetBuffering pipe NoBuffering
  pid <- getProcessID
  writeFile "/home/odin/.status_bar.pid" (show pid)
  _ <- installHandler sigTERM (Catch $ killHandler dPid) Nothing
  mainL pipe 0 0
