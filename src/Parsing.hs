module Parsing
    ( readOrThrow
    , parseClass
    , parseProperty
    , parseValidations
    , DslVal(..)
    , Property(..)
    , PropertyValidation(..)
    ) where

import Text.ParserCombinators.Parsec
import Data.Functor (void)

readOrThrow :: Parser a -> String -> Either String a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> Left $ show err
    Right val -> Right val

data PropertyValidation = NotNull
                        | MaxLength Integer
                        | MinLength Integer
                        deriving Show            

data Property = Property String String [PropertyValidation]
            deriving Show

data DslVal = Class String [Property]
            deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parsePropertyName :: Parser String
parsePropertyName = do
                    first <- letter
                    rest <- many (letter <|> digit <|> symbol)
                    return (first:rest)
                    
parsePropertyType :: Parser String
parsePropertyType = do
                    first <- letter
                    rest <- many (letter <|> digit <|> symbol)
                    return (first:rest)

parseInt :: Parser Integer
parseInt = do 
            digits <- many1 digit
            return $ read digits

parseValidationName :: Parser String
parseValidationName = do
                    first <- letter
                    rest <- many (letter <|> digit <|> symbol)
                    return (first:rest)

parseMaxLength :: Parser PropertyValidation
parseMaxLength = do 
                    spaces
                    i <- parseInt 
                    return $ MaxLength i

parseMinLength :: Parser PropertyValidation
parseMinLength = do 
                    spaces
                    i <- parseInt 
                    return $ MinLength i

parseValidation :: Parser PropertyValidation
parseValidation = do
                    validationName <- parseValidationName
                    spaces
                    case validationName of
                        "NotNull" -> return NotNull
                        "MaxLength" -> parseMaxLength
                        "MinLength" -> parseMinLength
                        _ -> unexpected ("Unknow validation(" ++ validationName ++ ")")


optionalSectionRemovingSpaces :: a -> Parser a -> Parser a
optionalSectionRemovingSpaces emptyCase parseCases = (char '\n' >> return emptyCase) <|> (space >> optionalSectionRemovingSpaces emptyCase parseCases) <|> parseCases

parseValidations :: Parser [PropertyValidation]
parseValidations = optionalSectionRemovingSpaces [] (sepBy1 parseValidation (char ','))


parseProperty :: Parser Property
parseProperty = do
                    spaces
                    char 'p'
                    atLeastOneSpace
                    n <- parsePropertyName
                    atLeastOneSpace
                    t <- parsePropertyType
                    vs <- parseValidations
                    spaces
                    return $ Property n t vs


parseClassName :: Parser String
parseClassName = do
                    first <- letter
                    rest <- many (letter <|> digit <|> symbol)
                    return (first:rest)

atLeastOneSpace :: Parser ()
atLeastOneSpace = skipMany1 space

parseBeginClass :: Parser ()
parseBeginClass = do
                    many space
                    char 'c'
                    atLeastOneSpace

parseClass :: Parser DslVal
parseClass = do
                parseBeginClass
                n <- parseClassName
                atLeastOneSpace
                ps <- manyTill parseProperty eof
                spaces
                return $ Class n ps