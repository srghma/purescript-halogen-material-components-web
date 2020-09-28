module Demo.RouteToPage where

import Protolude
import Halogen (Component) as H

import Demo.Pages.Index          as Demo.Pages.Index
import Demo.Pages.Button         as Demo.Pages.Button
-- | import Demo.Pages.Card           as Demo.Pages.Card
-- | import Demo.Pages.Checkbox       as Demo.Pages.Checkbox
-- | import Demo.Pages.Chip           as Demo.Pages.Chip
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
import Demo.Pages.TextField      as Demo.Pages.TextField
-- | import Demo.Pages.TopAppBar      as Demo.Pages.TopAppBar
import Demo.Pages.Typography     as Demo.Pages.Typography

-- | import Demo.Pages.TopAppBarPages.Standard       as Demo.Pages.TopAppBarPages.Standard
-- | import Demo.Pages.TopAppBarPages.Fixed          as Demo.Pages.TopAppBarPages.Fixed
-- | import Demo.Pages.TopAppBarPages.Dense          as Demo.Pages.TopAppBarPages.Dense
-- | import Demo.Pages.TopAppBarPages.Prominent      as Demo.Pages.TopAppBarPages.Prominent
-- | import Demo.Pages.TopAppBarPages.Short          as Demo.Pages.TopAppBarPages.Short
-- | import Demo.Pages.TopAppBarPages.ShortCollapsed as Demo.Pages.TopAppBarPages.ShortCollapsed
-- | import Demo.Pages.DrawerPages.Permanent         as Demo.Pages.DrawerPages.Permanent
-- | import Demo.Pages.DrawerPages.Dismissible       as Demo.Pages.DrawerPages.Dismissible
-- | import Demo.Pages.DrawerPages.Modal             as Demo.Pages.DrawerPages.Modal

import Demo.HOC.CatalogPage   as Demo.HOC.CatalogPage
-- | import Demo.HOC.DrawerPage    as Demo.HOC.DrawerPage
import Demo.HOC.TopAppBarPage as Demo.HOC.TopAppBarPage

import Demo.Route as Demo.Route

pagesRec :: Demo.Route.PagesRec (H.Component (Const Void) Unit Void Aff)
pagesRec =
  { "Index":                    Demo.Pages.Index.component
  , "Buttons":                  Demo.HOC.CatalogPage.mkComponent Demo.Route.Buttons        Demo.Pages.Button.config
  -- | , "Card":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.Card           Demo.Pages.Card.config
  -- | , "Checkbox":                 Demo.HOC.CatalogPage.mkComponent Demo.Route.Checkbox       Demo.Pages.Checkbox.config
  -- | , "Chips":                    Demo.HOC.CatalogPage.mkComponent Demo.Route.Chips          Demo.Pages.Chip.config
  -- | , "DataTable":                Demo.HOC.CatalogPage.mkComponent Demo.Route.DataTable      Demo.Pages.DataTable.config
  -- | , "Dialog":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Dialog         Demo.Pages.Dialog.config
  -- | , "Drawer":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Drawer         Demo.Pages.Drawer.config
  -- | , "Elevation":                Demo.HOC.CatalogPage.mkComponent Demo.Route.Elevation      Demo.Pages.Elevation.config
  -- | , "Fab":                      Demo.HOC.CatalogPage.mkComponent Demo.Route.Fab            Demo.Pages.Fab.config
  -- | , "IconButton":               Demo.HOC.CatalogPage.mkComponent Demo.Route.IconButton     Demo.Pages.IconButton.config
  -- | , "ImageList":                Demo.HOC.CatalogPage.mkComponent Demo.Route.ImageList      Demo.Pages.ImageList.config
  -- | , "LayoutGrid":               Demo.HOC.CatalogPage.mkComponent Demo.Route.LayoutGrid     Demo.Pages.LayoutGrid.config
  -- | , "List":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.List           Demo.Pages.List.config
  -- | , "LinearProgress":           Demo.HOC.CatalogPage.mkComponent Demo.Route.LinearProgress Demo.Pages.LinearProgress.config
  -- | , "Menu":                     Demo.HOC.CatalogPage.mkComponent Demo.Route.Menu           Demo.Pages.Menu.config
  -- | , "RadioButton":              Demo.HOC.CatalogPage.mkComponent Demo.Route.RadioButton    Demo.Pages.RadioButton.config
  -- | , "Ripple":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Ripple         Demo.Pages.Ripple.config
  -- | , "Select":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Select         Demo.Pages.Select.config
  -- | , "Slider":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Slider         Demo.Pages.Slider.config
  -- | , "Snackbar":                 Demo.HOC.CatalogPage.mkComponent Demo.Route.Snackbar       Demo.Pages.Snackbar.config
  -- | , "Switch":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.Switch         Demo.Pages.Switch.config
  -- | , "TabBar":                   Demo.HOC.CatalogPage.mkComponent Demo.Route.TabBar         Demo.Pages.TabBar.config
  , "TextField":                Demo.HOC.CatalogPage.mkComponent Demo.Route.TextField      Demo.Pages.TextField.config
  -- | , "Theme":                    Demo.HOC.CatalogPage.mkComponent Demo.Route.Theme          Demo.Pages.Theme.config
  -- | , "TopAppBar":                Demo.HOC.CatalogPage.mkComponent Demo.Route.TopAppBar      Demo.Pages.TopAppBar.config
  , "Typography":               Demo.HOC.CatalogPage.mkComponent Demo.Route.Typography     Demo.Pages.Typography.config
  -- | , "TopAppBar_Standard":       Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.Standard.config
  -- | , "TopAppBar_Fixed":          Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.Fixed.config
  -- | , "TopAppBar_Dense":          Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.Dense.config
  -- | , "TopAppBar_Prominent":      Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.Prominent.config
  -- | , "TopAppBar_Short":          Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.Short.config
  -- | , "TopAppBar_ShortCollapsed": Demo.HOC.TopAppBarPage.mkComponent Demo.Pages.TopAppBarPages.ShortCollapsed.config
  -- | , "Drawer_Permanent":         Demo.HOC.DrawerPage.mkComponent Demo.Pages.DrawerPages.Permanent.config
  -- | , "Drawer_Dismissible":       Demo.HOC.DrawerPage.mkComponent Demo.Pages.DrawerPages.Dismissible.config
  -- | , "Drawer_Modal":             Demo.HOC.DrawerPage.mkComponent Demo.Pages.DrawerPages.Modal.config
  }

routeToPage :: Demo.Route.Route -> H.Component (Const Void) Unit Void Aff
routeToPage route = Demo.Route.extractFromPagesRec route pagesRec
