#! /usr/bin/runhaskell

import Data.Time
import System.Process
import System.IO
import System.Locale
import Numeric
import System.Posix.Unistd
import qualified Network.MPD as MPD
import qualified Data.Map as M

-- Chromo
increment = 4.25
range = [0.0 .. 60.0]
cl = map (\i -> round $ i * increment) range

chromo secs | secs >=0 && secs < 239       = (0, 255, a)
            | secs == 240                  = (0, 255, 0)
            | secs >= 241 && secs <= 479   = (b, 255, 0)
            | secs == 480                  = (255, 255, 0)
            | secs >= 481 && secs <= 719   = (255, c, 0)
            | secs == 720                  = (255, 0, 0)
            | secs >= 721 && secs <= 959   = (255, 0, d)
            | secs == 960                  = (255, 0, 255)
            | secs >= 961 && secs <= 1199  = (e, 0 ,255)
            | secs == 1200                 = (0, 0, 255)
            | secs >= 1201 && secs <= 1439 = (0, f, 255)
            | otherwise                    = (255, 255, 255)
            where a = (cl !! (60 - (secs `div` 4)))
                  b = (cl !! ((secs - 240) `div` 4))
                  c = (cl !! (60 - (secs - 480) `div` 4))
                  d = (cl !! ((secs - 720) `div` 4))
                  e = cl !! ((60 - (secs - 960)) `div` 4)
                  f = cl !! ((secs - 1200) `div` 4)

-- Dzen settings
display_settings =  " -xs 1 -y -1 -ta l -sa c"
font = " -fn Terminus-10"
actions = " -e 'onstart=lower;button4=scrollup;button5=scrolldown;'"
dzenString = "dzen2 -p" ++ display_settings ++ actions ++ font

--wrap functions
wrapFG :: String -> String -> String
wrapFG str color = "^fg(" ++ color ++ ")" ++ str ++ "^fg()"
--wrapBG

toHexColor :: (Int, Int, Int) -> String
toHexColor (r, g, b) = "#" ++ red ++ green ++ blue
    where r'    = showHex r ""
          g'    = showHex g ""
          b'    = showHex b ""
          red   = if length r' == 1 then "0" ++ r' else r'
          green = if length g' == 1 then "0" ++ g' else g'
          blue  = if length b' == 1 then "0" ++ b' else b'

--mpd
mpd str = do
    test <- MPD.withMPD $ MPD.currentSong
    if isSong test
        then
            do
                Right (Just song) <- MPD.withMPD $ MPD.currentSong
                let tags = MPD.sgTags song
                let artist = head $ M.findWithDefault ["Artist"] MPD.Artist tags
                let title =  head $ M.findWithDefault ["Title"] MPD.Title tags
                return $ str ++ " MPD: " ++ title ++ " by " ++ artist
        else
            return str
    where isSong (Right (Just song)) = True
          isSong _                   = False

--time
time str = do
    now <- getZonedTime
    let timeStr = formatTime defaultTimeLocale "%A %B %e %I:%M:%S %p" now
    let local = zonedTimeToLocalTime now
    let tod = localTimeOfDay local
    let mins = ((todHour tod) * 60) + (todMin tod)
    let fg = toHexColor $ chromo mins
    return $ str ++ "DATE: " ++ (wrapFG timeStr fg)

-- display
display din = (time "")     >>=
              mpd           >>=
              hPutStrLn din >>
              hFlush din    >>
              sleep 1       >>
              display din






main = do
    (din, dout, derr, dps) <- runInteractiveCommand dzenString
    display din



