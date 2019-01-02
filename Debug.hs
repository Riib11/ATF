module Debug
( debug
) where

_DEBUG = False

debug :: String -> IO ()
debug msg = if _DEBUG
    then putStrLn $ "[/] " ++ msg
    else return ()
