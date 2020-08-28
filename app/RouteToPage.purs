module UIGuide.RouteToPage where

import Protolude
import Data.Const (Const(..))
import Halogen (Component) as H
import UIGuide.Pages.Button as UIGuide.Pages.Button
import UIGuide.Pages.Index as UIGuide.Pages.Index
import UIGuide.Route as UIGuide.Route

pagesRec :: UIGuide.Route.PagesRec (H.Component (Const Void) Unit Void Aff)
pagesRec =
  { "Index":  UIGuide.Pages.Index.component
  , "Button": UIGuide.Pages.Button.component
  }

routeToPage :: UIGuide.Route.Route -> H.Component (Const Void) Unit Void Aff
routeToPage route = UIGuide.Route.extractFromPagesRec route pagesRec
