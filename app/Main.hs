module Main where

import Parsing
import CSharpGen
import Language.CSharp.Pretty (prettyPrint)
import Language.CSharp.Lexer
import Language.CSharp.Parser (parser)
main :: IO ()
main =  
    -- parseCSharpCode
    generate


generate :: IO ()
generate =  do
    toParse <- readFile "test.dsl"
    case parseReadOrThrow toParse of
        Left err -> print err
        Right ast -> case transform ast of
                        Left err -> print err
                        Right code -> writeFile "test.cs" $ prettyPrint code
    print "end"


parseCSharpCode = do 
    let filename = "sample.cs"
    code <- readFile filename
    print $ parser filename $ lexer code