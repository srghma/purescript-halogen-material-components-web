module Demo.RouteToPage where

import Protolude
import Data.Const (Const(..))
import Halogen (Component) as H
import Demo.Pages.Buttons as Demo.Pages.Buttons
import Demo.HOC.CatalogPage as Demo.HOC.CatalogPage
import Demo.Pages.Index as Demo.Pages.Index
import Demo.Route as Demo.Route

pagesRec :: forall r w i . Demo.Route.PagesRec (H.Component (Const Void) Unit Void Aff)
pagesRec =
  { "Index":  Demo.Pages.Index.component
  , "Buttons": Demo.HOC.CatalogPage.mkComponent Demo.Route.Buttons Demo.Pages.Buttons.catalogPage
  }

routeToPage :: forall r w i . Demo.Route.Route -> H.Component (Const Void) Unit Void Aff
routeToPage route = Demo.Route.extractFromPagesRec route pagesRec
