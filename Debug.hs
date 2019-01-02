module Debug
( debug
) where

_DEBUG = True

debug :: String -> IO ()
debug msg = if _DEBUG
    then putStrLn $ "[/] " ++ msg
    else return ()
