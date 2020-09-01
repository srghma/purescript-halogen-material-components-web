module Demo.Route where

import Prelude
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as Routing.Duplex
import Routing.Duplex.Generic (noArgs, sum) as Routing.Duplex
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Index
  | Buttons
  | Card
  | Checkbox
  | Chips
  | DataTable
  | Dialog
  | Drawer
  | Elevation
  | Fab
  | IconButton
  | ImageList
  | LayoutGrid
  | List
  | LinearProgress
  | Menu
  | RadioButton
  | Ripple
  | Select
  | Slider
  | Snackbar
  | Switch
  | TabBar
  | TextField
  | Theme
  | TopAppBar
  | Typography

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

instance encodeJsonRoute :: EncodeJson Route where
  encodeJson = genericEncodeJson

instance decodeJsonRoute :: DecodeJson Route where
  decodeJson = genericDecodeJson

type PagesRec a
  = { "Index" :: a
    , "Buttons" :: a
    , "Card" :: a
    , "Checkbox" :: a
    , "Chips" :: a
    , "DataTable" :: a
    , "Dialog" :: a
    , "Drawer" :: a
    , "Elevation" :: a
    , "Fab" :: a
    , "IconButton" :: a
    , "ImageList" :: a
    , "LayoutGrid" :: a
    , "List" :: a
    , "LinearProgress" :: a
    , "Menu" :: a
    , "RadioButton" :: a
    , "Ripple" :: a
    , "Select" :: a
    , "Slider" :: a
    , "Snackbar" :: a
    , "Switch" :: a
    , "TabBar" :: a
    , "TextField" :: a
    , "Theme" :: a
    , "TopAppBar" :: a
    , "Typography" :: a
    }

-- NOTE: without a `Routing.Duplex.root $` to allow hashed routing
routeCodec :: Routing.Duplex.RouteDuplex' Route
routeCodec =
  Routing.Duplex.sum
    { "Index": Routing.Duplex.noArgs
    , "Buttons": "buttons" / Routing.Duplex.noArgs
    , "Card": "card" / Routing.Duplex.noArgs
    , "Checkbox": "checkbox" / Routing.Duplex.noArgs
    , "Chips": "chips" / Routing.Duplex.noArgs
    , "DataTable": "datatable" / Routing.Duplex.noArgs
    , "Dialog": "dialog" / Routing.Duplex.noArgs
    , "Drawer": "drawer" / Routing.Duplex.noArgs
    , "Elevation": "elevation" / Routing.Duplex.noArgs
    , "Fab": "fab" / Routing.Duplex.noArgs
    , "IconButton": "iconbutton" / Routing.Duplex.noArgs
    , "ImageList": "imagelist" / Routing.Duplex.noArgs
    , "LayoutGrid": "layoutgrid" / Routing.Duplex.noArgs
    , "List": "list" / Routing.Duplex.noArgs
    , "LinearProgress": "linearprogress" / Routing.Duplex.noArgs
    , "Menu": "menu" / Routing.Duplex.noArgs
    , "RadioButton": "radiobutton" / Routing.Duplex.noArgs
    , "Ripple": "ripple" / Routing.Duplex.noArgs
    , "Select": "select" / Routing.Duplex.noArgs
    , "Slider": "slider" / Routing.Duplex.noArgs
    , "Snackbar": "snackbar" / Routing.Duplex.noArgs
    , "Switch": "switch" / Routing.Duplex.noArgs
    , "TabBar": "tabbar" / Routing.Duplex.noArgs
    , "TextField": "textfield" / Routing.Duplex.noArgs
    , "Theme": "theme" / Routing.Duplex.noArgs
    , "TopAppBar": "topappbar" / Routing.Duplex.noArgs
    , "Typography": "typography" / Routing.Duplex.noArgs
    }

extractFromPagesRec :: forall a. Route -> PagesRec a -> a
extractFromPagesRec Index = _."Index"
extractFromPagesRec Buttons = _."Buttons"
extractFromPagesRec Card = _."Card"
extractFromPagesRec Checkbox = _."Checkbox"
extractFromPagesRec Chips = _."Chips"
extractFromPagesRec DataTable = _."DataTable"
extractFromPagesRec Dialog = _."Dialog"
extractFromPagesRec Drawer = _."Drawer"
extractFromPagesRec Elevation = _."Elevation"
extractFromPagesRec Fab = _."Fab"
extractFromPagesRec IconButton = _."IconButton"
extractFromPagesRec ImageList = _."ImageList"
extractFromPagesRec LayoutGrid = _."LayoutGrid"
extractFromPagesRec List = _."List"
extractFromPagesRec LinearProgress = _."LinearProgress"
extractFromPagesRec Menu = _."Menu"
extractFromPagesRec RadioButton = _."RadioButton"
extractFromPagesRec Ripple = _."Ripple"
extractFromPagesRec Select = _."Select"
extractFromPagesRec Slider = _."Slider"
extractFromPagesRec Snackbar = _."Snackbar"
extractFromPagesRec Switch = _."Switch"
extractFromPagesRec TabBar = _."TabBar"
extractFromPagesRec TextField = _."TextField"
extractFromPagesRec Theme = _."Theme"
extractFromPagesRec TopAppBar = _."TopAppBar"
extractFromPagesRec Typography = _."Typography"

toString :: Route -> String
toString route = "#" <> Routing.Duplex.print routeCodec route