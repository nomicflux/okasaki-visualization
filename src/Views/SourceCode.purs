module Views.SourceCode where

import Prelude (const)

import Pux.Html as H
import Pux.Html.Attributes as HA
import Pux.Html.Events as HE

import CodeSnippet as CS
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)

data CurrentFunction = None | Name String | All

type SourceCodeInfo = { sourceCode :: M.Map String String
                      , fullSource ::  Maybe String
                      , currFnName :: Maybe String
                      , currFn :: CurrentFunction
                      }

blankSourceCode :: SourceCodeInfo
blankSourceCode = { sourceCode : M.empty
                  , fullSource : Nothing
                  , currFnName : Nothing
                  , currFn : None
                  }

data CodeAction = LoadCode (Either String CS.SourceCode)
                | DisplaySource

changeFn :: SourceCodeInfo -> String -> SourceCodeInfo
changeFn code fn = code { currFn = maybe None Name (M.lookup fn code.sourceCode)
                        , currFnName = Just fn
                        }

updateCode :: CodeAction -> SourceCodeInfo -> SourceCodeInfo
updateCode (LoadCode (Left _)) code = code
updateCode (LoadCode (Right newCode)) oldCode =
  let
    code = oldCode { sourceCode = CS.parseFunctions newCode
                   , fullSource = Just (CS.getUncommentedCode newCode)
                   }
  in
   case code.currFnName of
     Nothing -> code
     Just fn -> changeFn code fn
updateCode DisplaySource code = code { currFn = All }

sourceBtn :: H.Html CodeAction
sourceBtn = H.div [ ] [ H.button [ HA.className "pure-button pure-button-success"
                                 , HE.onClick (const DisplaySource)
                                 ] [ H.text "Show Full Source Code"]]

viewCode :: SourceCodeInfo -> H.Html CodeAction
viewCode code =
  H.div [ ]
        [ H.code [ ]
          [ H.pre [ ]
            [ case code.currFn of
                 None ->
                   H.i []
                       [ H.text "No implementation given or no function selected"]
                 Name fn -> H.text fn
                 All ->
                   case code.fullSource of
                     Nothing -> H.i [] [ H.text "Full source not loaded yet" ]
                     Just fs ->
                       H.text fs
            ]
          ]
        ]
