module CodeSnippet where

import Data.Array as A
import Control.Monad.Aff (Aff, attempt)
import Data.Either (either, Either(..))
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.List (List(..))
import Data.Map (Map, fromFoldableWith)
-- import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (get, AJAX)
import Prelude (bind, pure, (<>), (<<<), show, ($), (*>), (<*), const, id, (<$>), unit)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (sepEndBy, manyTill, many, choice, lookAhead)
import Text.Parsing.StringParser.String (string, anyChar, noneOf, eof)

data Language = Purescript
              | Elm
              | Haskell
              | Idris
              | Clojure
              | Scheme
              | Elixir
              | Scala

derive instance eqLanguage :: Eq Language

comment :: Language -> String
comment Purescript = "--"
comment Elm = "--"
comment Haskell = "--"
comment Idris = "--"
comment Clojure = ";"
comment Scheme = ";"
comment Elixir = "#"
comment Scala = "//"

suffix :: Language -> String
suffix Purescript = "purs"
suffix Elm = "elm"
suffix Haskell = "hs"
suffix Idris = "idr"
suffix Clojure = "clj"
suffix Scheme = "scm"
suffix Elixir = "ex"
suffix Scala = "scala"

data SourceCode = SourceCode { getSourceCode :: String
                             , language :: Language
                             }
data FunctionBlock = FunctionBlock { name :: String
                                   , body :: String
                                   }

instance showSourceCode :: Show SourceCode where
  show (SourceCode sc) = sc.getSourceCode

instance showFunctionBlock :: Show FunctionBlock where
  show (FunctionBlock f) = f.name <> ": " <> f.body

getFile :: forall eff. String -> Language -> Aff (ajax :: AJAX | eff) (Either String SourceCode)
getFile fname lang = do
  let extension = suffix lang
      fullname = "/src/Structures/" <> extension <> "/" <> fname <> "." <> extension
  res <- attempt $ get fullname
  pure $ either (Left <<< show) (\r -> Right $ SourceCode { getSourceCode : r.response
                                                          , language : lang
                                                          }) res

docComment :: Language -> Parser String
docComment lang = string (comment lang <> " | *")

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< A.fromFoldable

functionTag :: Language -> Parser String
functionTag lang = fromCharList <$> (docComment lang *> many (noneOf ['\n']))

endComment :: Language -> Parser String
endComment lang = string (comment lang <> " .end")

functionBody :: Language -> Parser FunctionBlock
functionBody lang = do
  name <- functionTag lang
  body <- manyTill anyChar $ endComment lang
  pure $ FunctionBlock { name : name
                       , body : fromCharList body
                       }

functions :: Language -> Parser (List FunctionBlock)
functions lang = sepEndBy (functionBody lang) (nextFunction lang)

nextFunction :: Language -> Parser (List Char)
nextFunction lang = manyTill anyChar (choice [eof, (const unit <$> lookAhead (functionTag lang))])

parseFunctions :: SourceCode -> Map String String
parseFunctions (SourceCode code) =
  let
    res = runParser (nextFunction code.language *> functions code.language) code.getSourceCode
    fns = either (const Nil) id res
  in
   fromFoldableWith (\a b -> b <> a) (map (\ (FunctionBlock f) -> Tuple f.name f.body) fns)

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
testSC = SourceCode { getSourceCode: testString
                    , language: Purescript
                    }
