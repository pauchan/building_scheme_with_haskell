module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
  
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<>=?@^_-"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
                      Left err -> "No match: " ++ show err
                      Right val -> "found value"
spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many (noneOf "\"")
            char '"'
            return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    val <- many1 digit
    return $ Number $ read val

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
