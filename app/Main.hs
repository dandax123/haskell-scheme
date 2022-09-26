module Main where

import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec (sepBy)
import GHC.Float (fromRat'')
import GHC.Base (absentErr)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Float Float


instance Show LispVal where show = showVal
--  understoodf
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  let a = x ++ "." ++ y
  return $ Float $ read a

-- understood
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) (many1 digit)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> try parseFloat <|> parseNumber <|> parseQuoted <|> 
  do 
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

parseList ::  Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseQuoted:: Parser LispVal
parseQuoted  = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseDottedList :: Parser LispVal
parseDottedList  = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList  head tail



symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val


showVal :: LispVal -> String
showVal (String c) =  "\"" ++ c ++ "\""
showVal (Atom a) = a
showVal (Number a) = show a
showVal (Bool True)  = "#t"
showVal (Bool False)  = "#f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal



eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

main :: IO ()
main = do
  (expr : _) <- getArgs
  print $ ( eval . readExpr) expr
