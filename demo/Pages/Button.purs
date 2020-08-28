module Demo.Pages.Button where

import Prelude

import Data.Const (Const(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State
  = Unit

type Query
  = Const Void

type Action
  = Unit

type Input
  = Unit

type Message
  = Void

----------
-- HTML
component ::
  ∀ m. H.Component Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.div_
      [ HH.text "Button"
      ]
