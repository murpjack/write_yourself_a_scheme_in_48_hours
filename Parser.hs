module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read.Lex (Number)

main :: IO ()
main =
  print . eval . readExpr . head =<< getArgs

readExpr :: String -> LispVal
readExpr str = case parse parseExpr "lisp" str of
  Left err -> String $ "Error " ++ show err
  Right val -> val

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

eval :: LispVal -> LispVal
eval val@(Bool _) = val
eval val@(Number _) = val
eval val@(String _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

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
