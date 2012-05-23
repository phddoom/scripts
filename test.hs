{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (lines, unwords)
import Shelly
import Text.Printf
import Data.Text.Lazy as LT hiding (filter, any, map)
import Data.Text.Lazy.Read
default (LT.Text)

batteryPath = "/sys/class/power_supply/BAT0/"
chargeNowPath = batteryPath `append` "charge_now"
chargeFullPath = batteryPath `append` "charge_full"
memInfoPath = "/proc/meminfo" :: Text

data MemInfo = MemInfo {
    total :: Double
  , free  :: Double
  , buffers  :: Double
  , cached   :: Double
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
                        parseMem x = extractDouble . snd $ breakOnEnd ":" x

displayMemInfo :: MemInfo -> Text
displayMemInfo (MemInfo total free buffers cached) = display
                       where
                        used = total - (free + buffers + cached)
                        percent = used / total * 100
                        display = "MEM: " `append` printPercent percent

-- TODO: Clock, CPU, Sound, MPD
main = shelly $ verbosely $ do
  nowIO  <- readfile $ fromText chargeNowPath
  fullIO <- readfile $ fromText chargeFullPath
  memIO  <- readfile $ fromText memInfoPath

  let batteryText = displayBattery nowIO fullIO
  let memText     = displayMemInfo $ listToMemInfo $ filterMemInfo memIO

  echo $ unwords [batteryText, memText]
