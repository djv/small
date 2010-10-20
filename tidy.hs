import Language.Haskell.Exts
import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment

ind l n = if length l > n then Just $ l !! n else Nothing

parseModuleFromFile :: String -> Module
parseModuleFromFile inp = fromParseResult $ parseFileContents inp

main :: IO ()
main = do
    args <- getArgs
    let file = ind args 0
    let inp = maybe getContents readFile file
    inpStr <- inp
    let m = parseModuleFromFile inpStr
    putStrLn $ prettyPrint m
