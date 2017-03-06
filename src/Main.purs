module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.VDom.Driver (runUI)
import Halogen.Aff (HalogenEffects, runHalogenAff, awaitBody)
import Halogen.HTML as HH


type State = { world :: String }

initialState :: State
initialState = { world: "ZA WARUDO" }

data Query a = Get (String -> a)


komponen :: forall m. H.Component HH.HTML Query Unit Void m
komponen =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }


render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.h1_
      [ HH.text state.world ]
    ]

eval :: forall m. Query ~> H.ComponentDSL State Query Void m
eval (Get next) = do
  world <- H.gets $ _.world
  pure $ next world


main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff  $ do
  body <- awaitBody
  runUI komponen unit body
