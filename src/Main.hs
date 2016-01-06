import Data.Char
import System.Posix.Files
import System.Posix.Process

-- Edit below according to the location of your
-- battery charge information and
-- the desire timeout of the message.
currentPath = "/sys/class/power_supply/BATC/charge_now"
fullPath = "/sys/class/power_supply/BATC/charge_full"
alertTimeout = 1

-- Produces message given a percentage of battery remaining.
message :: Int -> String
message a = "Battery:\n\n" ++ show a ++ "% remaining."

-- Makes a alert using Zenity according to the string given.
alert :: String -> IO ()
alert m = executeFile "zenity" True
                [ "--timeout"
                , show alertTimeout
                , "--warning"
                , "--text"
                , "--display=:0.0"
                , m
                ]
                Nothing

-- Use this function for battery messages because boilerplate.
batteryAlert :: Float -> IO ()
batteryAlert a
    | a < 1.0 = alert $ message $ floor $ a * 100
    | otherwise = return ()

main = do
    c <- readFile currentPath
    f <- readFile fullPath
    let a = read $ unwords $ words c :: Float
    let b = read $ unwords $ words f :: Float
    let d = a / b
    batteryAlert d
