module Main where
import Check
import Parser
import System.IO
import Cps
import Lang
-- import Debug.Trace (trace)

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    src <- getLine
    let expr = parse1 src
    case expr of
        Left err -> print err
        Right (Left defs) -> do
            print defs
            case doInferDefs defs of
                Left err -> print err
                Right bs -> do
                    print bs
        Right (Right expr) -> do
            print expr
            case doInfer expr of
                Left err -> print err
                Right t -> do
                    print t
    main