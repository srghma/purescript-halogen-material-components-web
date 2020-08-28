module Main where

import Debug.Trace
import Protolude
import UIGuide.App
import UIGuide.Route

import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Global.Unsafe (unsafeDecodeURI, unsafeEncodeURI)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as H
import Halogen.VDom.Driver as H
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Routing.Hash as Routing
import Web.HTML.HTMLElement (HTMLElement)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- H.runUI app unit body
    liftEffect $ void $ matchesWith (\url -> traceId (parse routeCodec (traceId url))) \oldRoute newRoute ->
      when (oldRoute /= Just newRoute) $
      launchAff_ $ halogenIO.query (H.mkTell $ Navigate newRoute)
