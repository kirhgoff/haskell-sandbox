import System.Environment
import System.Exit
import Data.List
import Spiral

prettyPrint m = intercalate "\n" (map (intercalate "\t") mstr)
  where mstr = map (map show) m

main = do
  args <- getArgs
  case args of
    [] -> putStrLn $ "Exiting, need spiral size"
    list | length list > 1 -> putStrLn $ "too many parameters" 
    [sizeString] -> do
        let 
          size = read sizeString :: Integer
          matrix = spiral size
        putStrLn $ "spiral size " ++ (show size) ++ "\n" ++ prettyPrint matrix
