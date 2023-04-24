module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read.Lex (Number)

main :: IO ()
main =
  putStrLn . readExpr . head =<< getArgs

readExpr :: String -> String
readExpr str = case parse parseExpr "lisp" str of
  Left err -> "Error " ++ show err
  Right val -> "Found " ++ show val

data LispVal
  = Atom String
  | Array String
  | Bool Bool
  | DottedList [LispVal] LispVal
  | List [LispVal]
  | Number Integer
  | String String

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List l) = "(" ++ unwordsList l ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- TODO: Used for debugging
toString :: LispVal -> String
toString val =
  case val of
    Atom _ -> "Atom"
    Array _ -> "Array"
    Bool _ -> "Bool"
    DottedList _ _ -> "DottedList"
    List _ -> "List"
    Number _ -> "Number"
    String _ -> "String"

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDotted
      char ')'
      return x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- FIXME: the value "(dotted . list))" succeeds, but it should fail
parseDotted :: Parser LispVal
parseDotted = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]
