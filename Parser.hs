module Main where

import Control.Monad.Except
import Data.Functor
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Read.Lex (Number)

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled

readExpr :: String -> ThrowsError LispVal
readExpr str = case parse parseExpr "lisp" str of
  Left err -> throwError $ Parser err
  Right val -> return val

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)

showError :: LispError -> String
showError (NumArgs expected found) = "Expected: " ++ show expected ++ " args, found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected: " ++ show expected ++ ", but found " ++ show found
showError (Parser err) = "Parse error at " ++ show err
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default _) = "An error has occurred"

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

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval val@(String _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> (Number . foldl1 op)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
