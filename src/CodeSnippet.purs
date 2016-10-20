module CodeSnippet where

import Data.Array as A
import Control.Monad.Aff (Aff, attempt)
import Data.Either (either, Either(..))
import Data.Functor (map)
import Data.List (List(..))
import Data.Map (Map, lookup, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (bind, pure, (<>), (<<<), show, ($), (*>), (<*), const, id, (<$>), Unit, unit)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, sepEndBy, manyTill, many, choice, lookAhead)
import Text.Parsing.StringParser.String (string, skipSpaces, anyChar, noneOf, char, eof)

newtype SourceCode = SourceCode { getSourceCode :: String }
data FunctionBlock = FunctionBlock { name :: String
                                   , body :: String
                                   }

instance showSourceCode :: Show SourceCode where
  show (SourceCode sc) = sc.getSourceCode

instance showFunctionBlock :: Show FunctionBlock where
  show (FunctionBlock f) = f.name <> ": " <> f.body

getFile :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either String SourceCode)
getFile fname = do
  let fullname = "/src/Structures/" <> fname <> ".purs"
  res <- attempt $ get fullname
  pure $ either (Left <<< show) (\r -> Right $ SourceCode { getSourceCode : r.response}) res

docComment :: Parser String
docComment = string "-- | *"

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< A.fromFoldable

functionTag :: Parser String
functionTag = fromCharList <$> (docComment *> many (noneOf ['\n']))

endComment :: Parser String
endComment = string "-- .end"

functionBody :: Parser FunctionBlock
functionBody = do
  name <- functionTag
  body <- manyTill anyChar endComment
  pure $ FunctionBlock { name : name
                       , body : fromCharList body
                       }

functions :: Parser (List FunctionBlock)
functions = sepEndBy functionBody nextFunction

nextFunction :: Parser (List Char)
nextFunction = manyTill anyChar (choice [eof, (const unit <$> lookAhead functionTag)])

parseFunctions :: SourceCode -> Map String String
parseFunctions (SourceCode code) =
  let
    res = runParser (nextFunction *> functions) code.getSourceCode
    fns = either (const Nil) id res
  in
   fromFoldable (map (\ (FunctionBlock f) -> Tuple f.name f.body) fns)

testString :: String
testString = """
-- | *DataStructure Stack
data Stack a = Nil
             | Cons a (Stack a)
-- .end

-- | *empty
empty :: forall a. Stack a
empty = Nil
-- .end

-- | *head
head :: forall a. Stack a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x
-- .end

-- | *tail
tail :: forall a. Stack a -> Maybe (Stack a)
tail Nil = Nothing
tail (Cons _ xs) = Just xs
-- .end
"""

testSC :: SourceCode
testSC = SourceCode { getSourceCode: testString }
