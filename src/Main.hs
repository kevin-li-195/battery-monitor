import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import System.Environment
import System.Exit
import System.IO.Error
import System.Posix.Files
import System.Posix.Daemonize
import System.Posix.Types
import System.Posix.Process

-- | Edit below according to the location of your
-- battery charge information and
-- the desire timeout of the message.
currentPath = "/sys/class/power_supply/BATC/charge_now" :: FilePath
fullPath = "/sys/class/power_supply/BATC/charge_full" :: FilePath
-- allowAlertPath provides the $HOME/.battery-monitorinfo file path.
allowAlertPath = (++) <$> getEnv "HOME" <*> return "/.battery-monitorinfo" :: IO String
defaultAlertThreshold = 0.1 :: Float
-- alertTimeout = 5 :: Int

-- | Called when alert appears to prevent alert from appearing again if charge is still below threshold.
writeAllowAlert :: Bool -> IO ()
writeAllowAlert a
    | a == True = allowAlertPath >>= (\p -> writeFile p "1")
    | a == False = allowAlertPath >>= (\p -> writeFile p "0")

-- | Checks the current threshold file setting to determine whether an alert should be instantiated.
readAllowAlert :: IO Bool
readAllowAlert = do
    result <- tryIOError $ allowAlertPath >>= readFile >>= readIO
    case result of
        Right a -> do
            if a == 1 then
                return True
            else
                return False
        Left a -> do
            writeAllowAlert True
            return True

-- | Produces message given a percentage of battery remaining.
message :: Int -> String
message a = "Battery:\n\n" ++ show a ++ "% remaining."

-- | Makes a alert using Zenity according to the string given.
alert :: String -> IO ()
alert m = executeFile "zenity" True
                [ "--warning"
                , "--text"
                -- , "--timeout"
                -- , show alertTimeout
                , m
                ]
                Nothing

-- | Use this function for battery messages because boilerplate.
-- This function will alert at any level and then set allowAlert to False.
batteryAlert :: Float -> IO ()
batteryAlert a = do
    writeAllowAlert False
    forkProcess $ alert . message . floor $ a * 100
    return ()

formatPercent :: Float -> String
formatPercent = show . floor . (*100)

usage :: IO ()
usage = do
    putStrLn "\nUsage of battery-monitor: battery-monitor [threshold]\n"
    putStrLn "Running battery-monitor without a threshold will default to a 10% capacity alert threshold.\n"
    putStrLn "You should provide a number less than 1.0 too.\n"
    putStrLn "Example usage: battery-monitor 0.2"
    putStrLn "The above example will result in the battery monitor prompting an alert when the capacity falls below 20%.\n"
    putStrLn "Example usage: battery-monitor"
    putStrLn "The above example will result in the battery monitor prompting an alert when the capacity falls below 10%.\n"
    putStrLn "The .battery-monitorinfo file is used to preserve whether or not alerts are allowed so that you're not constantly alerted when capacity falls below the threshold.\n"
    putStrLn "WARNING: Running two instances of battery-monitor will result in repeated alerts.\n"
    exitFailure

-- | Core daemon running function.
daemon :: Float -> IO ()
daemon threshold = forever $ do
    threadDelay 60000000
    c <- readFile currentPath
    f <- readFile fullPath
    let a = read $ unwords $ words c :: Float
    let b = read $ unwords $ words f :: Float
    -- d is the current percentage charge. It is of type Float due to the type casting above.
    let d = a / b

    t <- readAllowAlert
    case t of
        True -> 
            -- If we allow an alert then we check if the current percentage
            -- charge is below the alert threshold.
            if d < threshold then do
                batteryAlert d
            else do
                return ()
        False -> do
            -- If we do not alert, then we check if we are above the alert threshold.
            -- That way, we'll be able to re-allow alerts when we go back above the
            -- alert threshold.
            if d > threshold then writeAllowAlert True
            else return ()

-- | Handler function will take care of command line mistakes.
handler :: [Float] -> IO ()
handler [] = do
    putStrLn "Running battery-monitor daemon with 10% alert threshold."
    daemonize $ daemon defaultAlertThreshold
handler [a] = do
    if a < 1.0 then do
        putStrLn $ "Running battery-monitor daemon with " ++ (formatPercent a) ++ "% alert threshold."
        daemonize $ daemon a
    else
        usage
handler (x:xs) = usage

main = do
    args <- tryIOError $ getArgs >>= (mapM readIO)
    case args of
        Left a -> (putStrLn $ show a) >> usage
        Right a -> handler a
