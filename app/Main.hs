module Main where
import Check
import Parser
import System.IO
import Cps
import Lang
import Common
import Text.Printf (printf)
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
            -- print defs
            case doInferDefs defs of
                Left err -> putStrLn err
                Right bs -> do
                    putStrLn $ printf "program: %s" (show bs)
                    let cl = l2clProg bs
                    putStrLn $ printf "core: %s" (show cl)
                    let cpsed = cpsProgram cl
                    putStrLn $ printf "cpsed: %s" (show cpsed)
        Right (Right expr) -> do
            -- print expr
            case doInfer expr of
                Left err -> putStrLn err
                Right (t, expr) -> do
                    putStrLn $ printf "type: %s" (show t)
                    let cl = l2cl emptyGamma expr
                    putStrLn $ printf "core: %s" (show cl)
                    let cpsed = cpsTopExprEmpytCtx cl
                    putStrLn $ printf "cpsed: %s" (show cpsed)
    main