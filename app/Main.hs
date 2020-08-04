module Main where
import Unification
import Parser
import System.IO
-- import Debug.Trace (trace)

main :: IO ()
main = do
    putStr "> "
    hFlush stdout
    src <- getLine
    let expr = parse1 src
    case expr of
        Left err -> print err
        Right (Left def) -> do
            print def
            case doInferDef def of
                Left err -> print err
                Right (t, _, _) -> print t
        Right (Right expr) -> do
            print expr
            case doInfer expr of
                Left err -> print err
                Right (t, _) -> print t
    main