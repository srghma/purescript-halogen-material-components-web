-- from https://github.com/rnons/purescript-halogen-storybook/

module Demo.App where

import Protolude
import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, launchAff_)
import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as H
import Routing.Hash as Routing
import Web.HTML.HTMLElement (HTMLElement)
import Debug.Trace
import Demo.Route
import Demo.RouteToPage

data Query a = Navigate Route a

type State = Route

type Action = Void

type Output = Void

type Input = Unit

type Slots = ( child :: H.Slot (Const Void) Void Route )

_child = SProxy :: SProxy "child"

app :: forall r w i . H.Component Query Input Output Aff
app =
  H.mkComponent
    { initialState: const Index
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  render :: forall r w i . State -> H.ComponentHTML Action Slots Aff
  render state = HH.slot _child state (routeToPage (spy "render" state)) unit absurd

  handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output Aff (Maybe a)
  handleQuery (Navigate route next) = do
    H.put (spy "queyr" route)
    pure (Just next)
