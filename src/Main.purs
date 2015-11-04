module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

type State = { world :: String }

initialState :: State
initialState = { world: "ZA WARUDO" }

data Query a = Get (String -> a)


ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text state.world ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Get next) = do
    world <- gets $ _.world
    pure $ next world


main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  appendToBody app.node
