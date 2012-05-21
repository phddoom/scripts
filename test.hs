{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Text.Printf
import Data.Text.Lazy as LT
import Data.Text.Lazy.Read
default (LT.Text)

{-monit_ = command_ "monit" ["-c", ".monitrc"]-}

battery_path = "/sys/class/power_supply/BAT0/"
charge_now_path = battery_path `append` "charge_now"
charge_full_path = battery_path `append` "charge_full"

displayBattery nowIO fullIO = "BAT: " `append` pack (printf "%0.2f"(now / full * 100))
                            where
                              getDec (Right x) = fst x
                              now  = getDec $ rational nowIO :: Double
                              full = getDec $ rational fullIO :: Double

main = shelly $ verbosely $ do
  {-listing <- cmd "ls" "-a" "."-}

  {-monit ["reload"]-}
  nowIO <- readfile $ fromText charge_now_path
  fullIO <- readfile $ fromText charge_full_path
  echo $ displayBattery nowIO fullIO
