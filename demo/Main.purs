module Main where

import Protolude
import Demo.App (Query(..), app)
import Demo.Route (routeCodec)
import Halogen (mkTell) as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI) as H
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- H.runUI app unit body
    liftEffect $ void
      $ matchesWith (parse routeCodec) \oldRoute newRoute ->
          when (oldRoute /= Just newRoute) $ launchAff_ $ halogenIO.query (H.mkTell $ Navigate newRoute)
