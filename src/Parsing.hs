module Parsing
    ( parseReadOrThrow
    , DslVal(..)
    , Class(..)
    , Property(..)
    , PropertyValidation(..)
    ) where

import Text.ParserCombinators.Parsec
import Data.Functor (void)

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
                        deriving (Show, Eq)

data Property = Property String String [PropertyValidation]
            deriving Show

data Class = Class String [Property]
            deriving Show

data DslVal = Namespace [String] String [Class]
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
                    rest <- many (letter <|> digit <|> symbol <|> char '[' <|> char ']' <|> char '.')
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

parseEquals :: Parser PropertyValidation
parseEquals = do 
                    spaces
                    n <- parsePropertyName 
                    return $ Equals n

parseValidation :: Parser PropertyValidation
parseValidation = do
                    validationName <- parseValidationName
                    spaces
                    case validationName of
                        "NotNull" -> return NotNull
                        "MaxLength" -> parseMaxLength
                        "MinLength" -> parseMinLength
                        "Required" -> return Required
                        "Equals" ->  parseEquals
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

parseClass :: Parser Class
parseClass = do
                n <- parseClassName
                atLeastOneSpace
                ps <- manyTill parseProperty (void (char 'c') <|> eof)
                spaces
                return $ Class n ps

parseBeginNamespace :: Parser ()
parseBeginNamespace = do
                    many space
                    char 'n'
                    atLeastOneSpace

parseNamespace :: Parser String
parseNamespace = do
                    first <- letter
                    rest <- many (letter <|> digit <|> symbol <|> char '.')
                    return (first:rest)

parseBeginUsings :: Parser ()
parseBeginUsings = do
                    many space
                    char 'u'
                    atLeastOneSpace

parseUsing :: Parser String
parseUsing = do
                spaces
                first <- letter
                rest <- many (letter <|> digit <|> symbol <|> char '.')
                spaces
                return (first:rest)

parseDslVal :: Parser DslVal
parseDslVal = do
                usings <- (parseBeginUsings >> between (char '[') (char ']') (sepBy1 parseUsing (char ','))) <|> return []
                parseBeginNamespace
                namespace <- parseNamespace
                spaces
                parseBeginClass
                cs <- manyTill parseClass eof
                spaces
                return $ Namespace usings namespace cs

