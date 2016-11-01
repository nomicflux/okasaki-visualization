module CodeSnippet where

-- import Data.Array as A
import Control.Monad.Aff (Aff, attempt)
import Control.Alternative ((<|>))
import Data.Either (either, Either(..))
import Data.Eq (class Eq)
import Data.Functor (map)
import Data.List (List(..), concatMap)
import Data.Map (Map, fromFoldableWith)
-- import Data.Maybe (Maybe(..))
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Network.HTTP.Affjax (get, AJAX)
import Network.HTTP.StatusCode (StatusCode(..))
import Prelude (bind, pure, (<>), (<<<), show, ($), (*>), (<*), const, id, (<$>), unit)
import Text.Parsing.Simple (Parser, parse, string, alphanum, sepBy, space, eof, char, word, manyChar, notFollowedBy, (>>), (<<), newline, isn't, anyOf, skip, ParseError)

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

getFile :: forall eff. String -> Language -> Aff (ajax :: AJAX | eff) (Either String SourceCode)
getFile fname lang = do
  let extension = suffix lang
      fullname = "/src/Structures/" <> extension <> "/" <> fname <> "." <> extension
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
    fns = either (\err -> spy $ Cons (errToFunctionBlock err) Nil) id res
  in
   spy $ fromFoldableWith (\a b -> b <> a) (concatMap (\ (FunctionBlock f) ->
                                                        let body = f.body <> "\n"
                                                        in  map (\ n -> Tuple n body) f.names)
                                            fns)

testString :: String
testString = """
-- | *Stack
data Stack a = Nil
             | Cons a (Stack a)
-- .end

-- | *empty head tail
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

testQueue :: String
testQueue = """
module Structures.Purs.Queue where

import Data.Semigroup (append)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Prelude ((&&))

import Structures.Purs.Stack as S

-- | *Queue
data Queue a = Queue { front :: S.Stack a
                     , back :: S.Stack a
                     }
-- .end

-- | *empty
empty :: forall a. Queue a
empty = Queue { front : S.empty
              , back : S.empty
              }
-- .end

-- | *isEmpty
isEmpty :: forall a. Queue a -> Boolean
isEmpty (Queue queue) =
  S.isEmpty queue.front && S.isEmpty queue.back
-- .end

-- | *rotate
rotate :: forall a. Queue a -> Queue a
rotate (Queue queue) =
  Queue { front : S.reverse queue.back
        , back : S.reverse queue.front
        }
-- .end

-- | *top
top :: forall a. Queue a -> Maybe a
top q@(Queue queue) =
  case S.head (queue.front) of
    Just x -> Just x
    Nothing ->
      case S.head (S.reverse queue.back) of
        Nothing -> Nothing
        Just y -> Just y
-- .end

-- | *back
back :: forall a. Queue a -> Maybe a
back (Queue queue) =
  case S.head (queue.back) of
    Just y -> Just y
    Nothing ->
      case S.head (S.reverse queue.front) of
        Nothing -> Nothing
        Just x -> Just x
-- .end

-- | *pop
pop :: forall a. Queue a -> Maybe (Queue a)
pop (Queue queue) =
  case S.tail (queue.front) of
    Just xs -> Just (Queue (queue { front = xs }))
    Nothing ->
      case S.tail (S.reverse queue.back) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { back = S.empty
                                      , front = ys}))
-- .end

-- | *eject
eject :: forall a. Queue a -> Maybe (Queue a)
eject (Queue queue) =
  case S.tail (queue.back) of
    Just xs -> Just (Queue (queue { back = xs }))
    Nothing ->
      case S.tail (S.reverse queue.front) of
        Nothing -> Nothing
        Just ys -> Just (Queue (queue { front = S.empty
                                      , back = ys}))
-- .end

-- | *push
push :: forall a. a -> Queue a -> Queue a
push val (Queue queue) = Queue (queue { front = S.cons val queue.front })
-- .end

-- | *inject
inject :: forall a. a -> Queue a -> Queue a
inject val (Queue queue) = Queue (queue { back = S.cons val queue.back })
-- .end

topHead :: forall a. Queue a -> Maybe a
topHead (Queue queue) = S.head queue.front

backHead :: forall a. Queue a -> Maybe a
backHead (Queue queue) = S.head queue.back

toStack :: forall a. Queue a -> S.Stack a
toStack (Queue queue) = append queue.front (S.reverse queue.back)

biCount :: forall a. Queue a -> Tuple Int Int
biCount (Queue queue) = Tuple (S.count queue.front) (S.count queue.back)

instance functorQueue :: Functor Queue where
  map f (Queue queue) = Queue (queue { front = map f queue.front, back = map f queue.back })

instance foldableQueue :: Foldable Queue where
  foldr f def queue = foldr f def (toStack queue)
  foldl f def queue = foldl f def (toStack queue)
  foldMap f queue = foldMap f (toStack queue)

instance showQueue :: (Show a) => Show (Queue a) where
  show (Queue queue) = (show queue.front) <> " // " <> (show queue.back)
"""
