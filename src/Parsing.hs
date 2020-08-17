module Parsing
    ( parseReadOrThrow
    , DslVal(..)
    , Class(..)
    , Property(..)
    , PropertyValidation(..)
    ) where

import Text.ParserCombinators.Parsec
import Data.Functor (void)
import Control.Applicative hiding ((<|>), optional, many)
import ParsingUtils

parseReadOrThrow :: String -> Either String DslVal
parseReadOrThrow = readOrThrow parseDslVal

readOrThrow :: Parser a -> String -> Either String a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> Left $ show err
    Right val -> Right val

data PropertyValidation = NotNull
                        | MaxLength Integer
                        | MinLength Integer
                        | Required
                        | Equals String
                        | NotEquals String
                        deriving (Show, Eq)

data Property = Property String String [PropertyValidation]
            deriving Show

data Class = Class String [Property]
            deriving Show

data DslVal = Namespace [String] String [Class]
            deriving Show

parseDslVal :: Parser DslVal
parseDslVal = Namespace <$> parseUsingsP <*> parseNamespaceP <*> parseClassP

parseUsingsP :: Parser [String]
parseUsingsP = option [] (keycharP 'u' *> betweenBracketsSepByComma parseUsing <* spaces)
    where 
        parseUsing = (:) <$> letter <*> many (letter <|> digit <|> symbol <|> char '.')

parseNamespaceP :: Parser String
parseNamespaceP = keycharP 'n' *> parseNamespace <* spaces
    where
        parseNamespace = (:) <$> letter <*> many (letter <|> digit <|> symbol <|> char '.')

classChar :: Char
classChar = 'c'

parseClassP :: Parser [Class]
parseClassP = keycharP classChar *> manyTill (lexeme parseClass) eof <* spaces
    
parseClass :: Parser Class
parseClass = Class <$> (ws *> parseClassName <* spaces) <*> parseProperties
    where
        parseClassName = (:) <$> letter <*> many (letter <|> digit <|> symbol)

parseProperties :: Parser [Property]
parseProperties = manyTill parseProperty (void (char classChar) <|> eof)

parseProperty :: Parser Property
parseProperty = keycharP 'p' *> (Property <$> keywordP parsePropertyName <*> lexeme parsePropertyType <*> parseValidations) <* spaces
    where
        parsePropertyName = (:) <$> letter <*> many (letter <|> digit <|> symbol)
        parsePropertyType = (:) <$> letter <*> many (letter <|> digit <|> symbol <|> char '[' <|> char ']' <|> char '.')
        parseValidations = option [] $ betweenBracketsSepByComma parseValidation 
        parseValidation = lexeme parseValidationName >>= stringToPropertyValidation
        stringToPropertyValidation validationName = 
            case validationName of
                "NotNull" -> return NotNull
                "MaxLength" -> parseMaxLength
                "MinLength" -> parseMinLength
                "Required" -> return Required
                "Equals" ->  parseEquals
                "NotEquals" -> parseNotEquals
                _ -> unexpected ("Unknow validation(" ++ validationName ++ ")")
        parseValidationName = (:) <$> letter <*> many (letter <|> digit <|> symbol)
        parseMaxLength = MaxLength <$> lexeme parseInt
        parseMinLength = MinLength <$> lexeme parseInt
        parseEquals = Equals <$> lexeme parsePropertyName
        parseNotEquals = NotEquals <$> lexeme parsePropertyName
