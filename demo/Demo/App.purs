-- from https://github.com/rnons/purescript-halogen-storybook/
module Demo.App where

import Protolude
import Halogen (Component, ComponentHTML, HalogenM, Slot, defaultEval, mkComponent, mkEval, put) as H
import Halogen.HTML as HH
import Demo.Route (Route(..))
import Demo.RouteToPage (routeToPage)
import Type.Proxy

data Query a
  = Navigate Route a

type State
  = Route

type Action
  = Void

type Output
  = Void

type Input
  = Unit

type Slots
  = ( child :: H.Slot (Const Void) Void Route )

_child = Proxy :: Proxy "child"

app :: H.Component Query Input Output Aff
app =
  H.mkComponent
    { initialState: const Index
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  render :: State -> H.ComponentHTML Action Slots Aff
  render state = HH.slot _child state (spy "app route to page" $ routeToPage state) unit absurd

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output Aff (Maybe a)
  handleQuery (Navigate route next) = do
    H.put route
    pure (Just next)
