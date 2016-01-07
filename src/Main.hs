import Data.Char
import System.Posix.Files
import System.Posix.Process

-- Edit below according to the location of your
-- battery charge information and
-- the desire timeout of the message.
currentPath = "/sys/class/power_supply/BATC/charge_now"
fullPath = "/sys/class/power_supply/BATC/charge_full"
allowAlertPath = "$HOME/.batterymonitorinfo"
alertThreshold = 0.15 :: Float
alertTimeout = 5

-- Called when alert appears to prevent alert from appearing again if charge is still below threshold.
writeAllowAlert :: Bool -> IO ()
writeAllowAlert
    | True = writeFile alertPath "1"
    | False = writeFile alertPath "0"

-- Checks the current threshold file setting to determine whether an alert should be instantiated.
readAllowAlert :: IO Bool
readAllowAlert = readFile alertPath >>= readIO

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
-- This function will alert at any level and then set allowAlert to False.
batteryAlert :: Float -> IO ()
batteryAlert a = do
    alert $ message $ floor $ a * 100
    writeAllowAlert False

main = do
    c <- readFile currentPath
    f <- readFile fullPath
    let a = read $ unwords $ words c :: Float
    let b = read $ unwords $ words f :: Float
    -- d is the current percentage charge. It is of type Float due to the type casting above.
    let d = a / b
    t <- readAllowAlert
    case t of
            True -> 
                -- If we do work LEFT OFF HERE CONTINUE HERE NOW
                if d < alertThreshold then do
                    batteryAlert d
                else
                    return ()
            False ->
                -- If we do not alert, then we check if we are above the alert threshold.
                if d > alertThreshold then writeAllowAlert True
            _ ->
                writeAllowAlert True
