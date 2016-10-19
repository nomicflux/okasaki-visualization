module CodeSnippet where

import Control.Monad.Aff (Aff, attempt)
import Data.Array as A
import Data.Either (either, Either(..))
import Data.Functor (map)
import Data.List (List(..))
import Data.Map (Map, lookup, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (bind, pure, (<>), (<<<), show, ($), (*>), (<*), const, id, (<$>))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.String (string, skipSpaces, anyChar, eof)
import Text.Parsing.StringParser.Combinators (many1, sepBy, manyTill, many)

newtype SourceCode = SourceCode { getSourceCode :: String }
data FunctionBlock = FunctionBlock { name :: String
                                   , body :: String
                                   }

getFile :: forall eff. String -> Aff (ajax :: AJAX | eff) (Either String SourceCode)
getFile fname = do
  let fullname = "src/Structures/" <> fname <> ".purs"
  res <- attempt $ get fullname
  pure $ either (Left <<< show) (\r -> Right $ SourceCode { getSourceCode : r.response}) res

docComment :: Parser String
docComment = string "-- | *"

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< A.fromFoldable

functionTag :: Parser String
functionTag = (fromCharList) <$> (docComment *> skipSpaces *> many1 anyChar)

endComment :: Parser String
endComment = string "-- .end"

functionBody :: Parser FunctionBlock
functionBody = do
  name <- functionTag
  body <- manyTill anyChar endComment
  _ <- endComment
  pure $ FunctionBlock { name : name
                       , body : fromCharList body
                       }

functions :: Parser (List FunctionBlock)
functions = sepBy functionBody (many anyChar)

parseFunctions :: SourceCode -> Map String String
parseFunctions (SourceCode code) =
  let
    res = runParser functions code.getSourceCode
    fns = either (const Nil) id res
  in
   fromFoldable (map (\ (FunctionBlock f) -> Tuple f.name f.body) fns)
