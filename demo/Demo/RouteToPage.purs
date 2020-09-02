module Demo.RouteToPage where

import Protolude
import Halogen (Component) as H
import Demo.Pages.Index as Demo.Pages.Index
import Demo.Pages.Buttons as Demo.Pages.Buttons
import Demo.Pages.Cards as Demo.Pages.Cards
import Demo.Pages.Checkbox as Demo.Pages.Checkbox
import Demo.Pages.Chips as Demo.Pages.Chips
-- | import Demo.Pages.DataTable      as Demo.Pages.DataTable
-- | import Demo.Pages.Dialog         as Demo.Pages.Dialog
-- | import Demo.Pages.Drawer         as Demo.Pages.Drawer
-- | import Demo.Pages.Elevation      as Demo.Pages.Elevation
-- | import Demo.Pages.Fab            as Demo.Pages.Fab
-- | import Demo.Pages.IconButton     as Demo.Pages.IconButton
-- | import Demo.Pages.ImageList      as Demo.Pages.ImageList
-- | import Demo.Pages.LayoutGrid     as Demo.Pages.LayoutGrid
-- | import Demo.Pages.List           as Demo.Pages.List
-- | import Demo.Pages.LinearProgress as Demo.Pages.LinearProgress
-- | import Demo.Pages.Menu           as Demo.Pages.Menu
-- | import Demo.Pages.RadioButton    as Demo.Pages.RadioButton
-- | import Demo.Pages.Ripple         as Demo.Pages.Ripple
-- | import Demo.Pages.Select         as Demo.Pages.Select
-- | import Demo.Pages.Slider         as Demo.Pages.Slider
-- | import Demo.Pages.Snackbar       as Demo.Pages.Snackbar
-- | import Demo.Pages.Switch         as Demo.Pages.Switch
-- | import Demo.Pages.TabBar         as Demo.Pages.TabBar
-- | import Demo.Pages.TextField      as Demo.Pages.TextField
-- | import Demo.Pages.Theme          as Demo.Pages.Theme
-- | import Demo.Pages.TopAppBar      as Demo.Pages.TopAppBar
-- | import Demo.Pages.Typography     as Demo.Pages.Typography
import Demo.HOC.CatalogPage as Demo.HOC.CatalogPage
import Demo.Route as Demo.Route

pagesRec :: Demo.Route.PagesRec (H.Component (Const Void) Unit Void Aff)
pagesRec = spy "pagesRec"
  { "Index":                    Demo.Pages.Index.component
  , "Buttons":                  Demo.HOC.CatalogPage.mkComponent Demo.Route.Buttons Demo.Pages.Buttons.catalogPage
  , "Card":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.Card Demo.Pages.Cards.catalogPage
  , "Checkbox":                 Demo.HOC.CatalogPage.mkComponent Demo.Route.Checkbox Demo.Pages.Checkbox.catalogPage
  , "Chips":                    Demo.HOC.CatalogPage.mkComponent Demo.Route.Chips Demo.Pages.Chips.catalogPage
  , "DataTable":                Demo.HOC.CatalogPage.mkComponent Demo.Route.DataTable Demo.Pages.Buttons.catalogPage
  , "Dialog":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Dialog Demo.Pages.Buttons.catalogPage
  , "Drawer":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Drawer Demo.Pages.Buttons.catalogPage
  , "Elevation":                Demo.HOC.CatalogPage.mkComponent Demo.Route.Elevation Demo.Pages.Buttons.catalogPage
  , "Fab":                      Demo.HOC.CatalogPage.mkComponent Demo.Route.Fab Demo.Pages.Buttons.catalogPage
  , "IconButton":               Demo.HOC.CatalogPage.mkComponent Demo.Route.IconButton Demo.Pages.Buttons.catalogPage
  , "ImageList":                Demo.HOC.CatalogPage.mkComponent Demo.Route.ImageList Demo.Pages.Buttons.catalogPage
  , "LayoutGrid":               Demo.HOC.CatalogPage.mkComponent Demo.Route.LayoutGrid Demo.Pages.Buttons.catalogPage
  , "List":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.List Demo.Pages.Buttons.catalogPage
  , "LinearProgress":           Demo.HOC.CatalogPage.mkComponent Demo.Route.LinearProgress Demo.Pages.Buttons.catalogPage
  , "Menu":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.Menu Demo.Pages.Buttons.catalogPage
  , "RadioButton":              Demo.HOC.CatalogPage.mkComponent Demo.Route.RadioButton Demo.Pages.Buttons.catalogPage
  , "Ripple":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Ripple Demo.Pages.Buttons.catalogPage
  , "Select":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Select Demo.Pages.Buttons.catalogPage
  , "Slider":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Slider Demo.Pages.Buttons.catalogPage
  , "Snackbar":                 Demo.HOC.CatalogPage.mkComponent Demo.Route.Snackbar Demo.Pages.Buttons.catalogPage
  , "Switch":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Switch Demo.Pages.Buttons.catalogPage
  , "TabBar":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.TabBar Demo.Pages.Buttons.catalogPage
  , "TextField":                Demo.HOC.CatalogPage.mkComponent Demo.Route.TextField Demo.Pages.Buttons.catalogPage
  , "Theme":                    Demo.HOC.CatalogPage.mkComponent Demo.Route.Theme Demo.Pages.Buttons.catalogPage
  , "TopAppBar":                Demo.HOC.CatalogPage.mkComponent Demo.Route.TopAppBar Demo.Pages.Buttons.catalogPage
  , "Typography":               Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_Standard":       Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_Fixed":          Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_Dense":          Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_Prominent":      Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_Short":          Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  , "TopAppBar_ShortCollapsed": Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography Demo.Pages.Buttons.catalogPage
  }

routeToPage :: Demo.Route.Route -> H.Component (Const Void) Unit Void Aff
routeToPage route = Demo.Route.extractFromPagesRec route pagesRec
