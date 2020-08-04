module Main where

import Parsing
import CSharpGen
import Language.CSharp.Pretty (prettyPrint)

main :: IO ()
main =  do
    toParse <- readFile "test.dsl"
    -- print $ show $ readOrThrow parseValidations " NotNull , V\n"
    case readOrThrow parseClass toParse of
        Left err -> print err
        Right ast -> case transform ast of
                        Left err -> print err
                        Right code -> writeFile "test.cs" $ prettyPrint code
    print "end"


