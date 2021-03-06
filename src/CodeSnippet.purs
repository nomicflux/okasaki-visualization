module CodeSnippet where

-- import Data.Array as A
import Control.Monad.Aff (Aff, attempt)
import Control.Alternative ((<|>))
import Data.Array (filter)
import Data.Either (either, Either(..))
import Data.Eq (class Eq)
import Data.Foldable (fold)
import Data.Functor (map)
import Data.List (List(..), concatMap)
import Data.Map (Map, fromFoldableWith)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.String (joinWith, split, contains)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (get, AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (bind, pure, (<>), (<<<), show, ($), (*>), (<*), const, id, (<$>), unit, not)
import Text.Parsing.Simple (Parser, parse, string, alphanum, sepBy, space, eof, char, word, manyChar, notFollowedBy, (>>), (<<), newline, isn't, anyOf, ParseError)

type ParserS = Parser String

data Language = Purescript
              | Elm
              | Haskell
              | Idris
              | Clojure
              | Scheme
              | Elixir
              | Scala

derive instance eqLanguage :: Eq Language
derive instance ordLanguage :: Ord Language

allLangs :: Array Language
allLangs = [ Purescript
           , Elm
           , Haskell
           , Idris
           , Clojure
           , Scheme
           , Elixir
           , Scala
           ]

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

codeClass :: Language -> String
codeClass Purescript = "haskell"
codeClass Elm = "elm"
codeClass Haskell = "haskell"
codeClass Idris = "haskell"
codeClass Clojure = "clojure"
codeClass Scheme = "scheme"
codeClass Elixir = "elixir"
codeClass Scala = "scala"

stringToLang :: String -> Maybe Language
stringToLang str =
  case str of
    "purs" -> Just Purescript
    "elm" -> Just Elm
    "hs" -> Just Haskell
    "idr" -> Just Idris
    "clj" -> Just Clojure
    "scm" -> Just Scheme
    "ex" -> Just Elixir
    "scala" -> Just Scala
    _ -> Nothing

data SourceCode = SourceCode { getSourceCode :: String
                             , language :: Language
                             }
data FunctionBlock = FunctionBlock { names :: List String
                                   , body :: String
                                   }

instance showSourceCode :: Show SourceCode where
  show (SourceCode sc) = sc.getSourceCode

instance showFunctionBlock :: Show FunctionBlock where
  show (FunctionBlock f) = show f.names <> ": " <> f.body

getLanguage :: SourceCode -> Language
getLanguage (SourceCode code) = code.language

getFile :: forall eff. String -> Language -> Aff (ajax :: AJAX | eff) (Either String SourceCode)
getFile fname lang = do
  let extension = suffix lang
      fullname = "src/Structures/" <> extension <> "/" <> fname <> "." <> extension
  res <- attempt $ get fullname
  pure $ either (Left <<< show)
    (\r -> case r.status of
        StatusCode 200 -> Right $ SourceCode { getSourceCode : r.response
                                  , language : lang
                                  }
        StatusCode status -> Left $ show status) res

docComment :: Language -> ParserS String
docComment lang = string (comment lang <> " | *")

words :: ParserS (List String)
words = sepBy word space

functionTag :: Language -> ParserS (List String)
functionTag lang = docComment lang *> words <* (eof <|> (const unit <$> char '\n'))

endComment :: Language -> ParserS String
endComment lang = string (comment lang <> " .end")

anyChar :: ParserS Char
anyChar = alphanum <|> space <|> newline <|> anyOf "=:-+*!@$%^(){}[]\"\'`~|/.,;?<>_#&"

notBeginning :: Language -> ParserS String
notBeginning lang =
  manyChar ((isn't $ docComment lang) >> anyChar)

notEnd :: Language -> ParserS String
notEnd lang = manyChar ((notFollowedBy $ endComment lang) >> anyChar)

functionBody :: Language -> ParserS FunctionBlock
functionBody lang = do
  names <- functionTag lang
  body <- notEnd lang
  endComment lang
  pure $ FunctionBlock { names : names
                       , body : body
                       }

getUncommentedCode :: SourceCode -> String
getUncommentedCode (SourceCode code) =
  let
    lines = split "\n" code.getSourceCode
    com = comment code.language
    uncomment = filter (\line -> not (contains com line)) lines
  in
   joinWith "\n" uncomment

functions :: Language -> ParserS (List FunctionBlock)
functions lang = sepBy (functionBody lang) (notBeginning lang)

errToFunctionBlock :: ParseError -> FunctionBlock
errToFunctionBlock err =
  FunctionBlock { names : Cons "errorMsg" Nil
                , body : err
                }

parseFunctions :: SourceCode -> Map String String
parseFunctions (SourceCode code) =
  let
    res = parse (notBeginning code.language >> functions code.language) code.getSourceCode
    fns = either (\err -> Cons (errToFunctionBlock err) Nil) id res
  in
   fromFoldableWith (\a b -> b <> a) (concatMap (\ (FunctionBlock f) ->
                                                  let body = f.body <> "\n"
                                                  in  map (\ n -> Tuple n body) f.names)
                                      fns)
