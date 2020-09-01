module Demo.HOC.CatalogPage where

import Material.Classes.Drawer (mdc_drawer__content, mdc_drawer_app_content, mdc_list_item__graphic)
import Material.Classes.TopAppBar (mdc_top_app_bar____fixed_adjust, mdc_top_app_bar__navigation_icon, mdc_top_app_bar__row, mdc_top_app_bar__section, mdc_top_app_bar__section____align_start, mdc_top_app_bar__title)
import Material.Classes.Typography (mdc_typography, mdc_typography____body1, mdc_typography____headline5, mdc_typography____headline6)
import Protolude (Aff, Const, Maybe(..), SProxy(..), Unit, Void, absurd, const, map, not, unit, (<>), (==))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Demo.Route (Route)
import Demo.Route as Route
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Drawer.Dismissible as Drawer.Dismissible
import HalogenMWC.IconButton as IconButton
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import HalogenMWC.TopAppBar as TopAppBar
import MaterialIconsFont.Classes (material_icons)

type CatalogPage
  = { title :: String
    , prelude :: String
    , resources ::
      { materialDesignGuidelines :: Maybe String
      , documentation :: Maybe String
      , sourceCode :: Maybe String
      }
    , hero :: H.Component (Const Void) Unit Void Aff
    , content :: H.Component (Const Void) Unit Void Aff
    }

type State
  = { isDrawerOpen :: Boolean }

type Query
  = Const Void

type ChildSlots
  = ( hero :: H.Slot (Const Void) Void Unit
    , content :: H.Slot (Const Void) Void Unit
    )

data Action
  = Toggle

type Input
  = Unit

type Message
  = Void

mkComponent :: Route -> CatalogPage -> H.Component Query Input Message Aff
mkComponent routeOfThisCatalogPage catalogPage =
  H.mkComponent
    { initialState: const { isDrawerOpen: false }
    , render: render routeOfThisCatalogPage catalogPage
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  Toggle -> H.modify_ \state -> state { isDrawerOpen = not state.isDrawerOpen }

render :: Route -> CatalogPage -> State -> HH.ComponentHTML Action ChildSlots Aff
render routeOfThisCatalogPage catalogPage state =
  HH.div
    catalogPageContainer
    [ TopAppBar.topAppBar TopAppBar.defaultConfig
        [ HH.section
            [ HP.class_ mdc_top_app_bar__row ]
            [ HH.section
                [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ] ]
                [ IconButton.iconButton
                    ( IconButton.defaultConfig
                        { additionalClasses = [ mdc_top_app_bar__navigation_icon, material_icons ]
                        , additionalAttributes = [ HE.onClick (const Toggle) ]
                        }
                    )
                    [ HH.text "menu" ]
                , HH.span
                    [ HP.class_ mdc_top_app_bar__title
                    , HP.style "text-transform: uppercase; font-weight: 400;"
                    ]
                    [ HH.text "Material Components for Halogen" ]
                ]
            ]
        ]
    , HH.div demoPanel
        [ Drawer.Dismissible.drawer
            ( Drawer.Dismissible.defaultConfig
                { open = state.isDrawerOpen
                , additionalClasses = [ mdc_top_app_bar____fixed_adjust ]
                , additionalAttributes = [ HP.style "z-index: 1;" ]
                }
            )
            [ HH.div
                [ HP.class_ mdc_drawer__content ]
                [ let
                    processItem { route, label } =
                      List.Item.listItem
                        ( List.Item.defaultConfig
                            { selected = if routeOfThisCatalogPage == route then Just List.Item.Activated else Nothing
                            , href = Just (Route.toString route)
                            }
                        )
                        [ HH.text label ]
                  in
                    case Array.uncons (map processItem catalogDrawerItems) of
                      Just { head: listItem, tail: listItems } -> List.list List.defaultConfig listItem listItems
                      _ -> HH.text ""
                ]
            ]
        , HH.div
            ([ HP.classes [ mdc_top_app_bar____fixed_adjust, mdc_drawer_app_content ] ] <> demoContent)
            [ HH.div demoContentTransition
                [ HH.h1 [ HP.class_ mdc_typography____headline5 ] [ HH.text catalogPage.title ]
                , HH.p [ HP.class_ mdc_typography____body1 ] [ HH.text catalogPage.prelude ]
                , HH.slot (SProxy :: SProxy "hero") unit catalogPage.hero unit absurd
                , HH.h2 ([ HP.class_ mdc_typography____headline6 ] <> demoTitle) [ HH.text "Resources" ]
                , resourcesList catalogPage.resources
                , HH.h2 ([ HP.class_ mdc_typography____headline6 ] <> demoTitle) [ HH.text "Demos" ]
                , HH.slot (SProxy :: SProxy "content") unit catalogPage.content unit absurd
                ]
            ]
        ]
    ]

resourcesList ::
  forall r w i.
  { materialDesignGuidelines :: Maybe String
  , documentation :: Maybe String
  , sourceCode :: Maybe String
  } ->
  HH.HTML w i
resourcesList { materialDesignGuidelines, documentation, sourceCode } =
  List.list List.defaultConfig
    ( List.Item.listItem
        ( List.Item.defaultConfig
            { href = materialDesignGuidelines
            , target = Just "_blank"
            }
        )
        [ HH.div ([ HP.class_ mdc_list_item__graphic ] <> resourcesGraphic)
            [ HH.img ([ HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_material_design_24px.svg" ] <> resourcesGraphic)
            ]
        , HH.text "Material Design Guidelines"
        ]
    )
    [ List.Item.listItem
        ( List.Item.defaultConfig
            { href = documentation
            , target = Just "_blank"
            }
        )
        [ HH.div
            ([ HP.class_ mdc_list_item__graphic ] <> resourcesGraphic)
            [ HH.img ([ HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_drive_document_24px.svg" ] <> resourcesGraphic)
            ]
        , HH.text "Documentation"
        ]
    , List.Item.listItem
        ( List.Item.defaultConfig
            { href = sourceCode
            , target = Just "_blank"
            }
        )
        [ HH.div
            ([ HP.class_ mdc_list_item__graphic ] <> resourcesGraphic)
            [ HH.img ([ HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_code_24px.svg" ] <> resourcesGraphic)
            ]
        , HH.text "Source Code"
        ]
    ]

catalogDrawerItems :: forall r w i. Array { label :: String, route :: Route }
catalogDrawerItems =
  [ { label: "Home", route: Route.Index }
  , { label: "Button", route: Route.Buttons }
  , { label: "Card", route: Route.Card }
  , { label: "Checkbox", route: Route.Checkbox }
  , { label: "Chips", route: Route.Chips }
  , { label: "DataTable", route: Route.DataTable }
  , { label: "Dialog", route: Route.Dialog }
  , { label: "Drawer", route: Route.Drawer }
  , { label: "Elevation", route: Route.Elevation }
  , { label: "FAB", route: Route.Fab }
  , { label: "Icon Button", route: Route.IconButton }
  , { label: "Image List", route: Route.ImageList }
  , { label: "Layout Grid", route: Route.LayoutGrid }
  , { label: "Linear Progress Indicator", route: Route.LinearProgress }
  , { label: "List", route: Route.List }
  , { label: "Menu", route: Route.Menu }
  , { label: "Radio Button", route: Route.RadioButton }
  , { label: "Ripple", route: Route.Ripple }
  , { label: "Select", route: Route.Select }
  , { label: "Slider", route: Route.Slider }
  , { label: "Snackbar", route: Route.Snackbar }
  , { label: "Switch", route: Route.Switch }
  , { label: "Tab Bar", route: Route.TabBar }
  , { label: "Text Field", route: Route.TextField }
  , { label: "Theme", route: Route.Theme }
  , { label: "Top App Bar", route: Route.TopAppBar }
  , { label: "Typography", route: Route.Typography }
  ]

catalogPageContainer :: forall r w i. Array (IProp I.HTMLdiv i)
catalogPageContainer =
  [ HP.style "position: relative;"
  , HP.class_ mdc_typography
  ]

demoPanel :: forall r w i. Array (IProp I.HTMLdiv i)
demoPanel =
  [ HP.style "display: -ms-flexbox; display: flex; position: relative; height: 100vh; overflow: hidden;"
  ]

demoContent :: forall r w i. Array (IProp I.HTMLdiv i)
demoContent =
  [ HP.style "height: 100%; -webkit-box-sizing: border-box; box-sizing: border-box; max-width: 100%; padding-left: 16px; padding-right: 16px; padding-bottom: 100px; width: 100%; overflow: auto; display: -ms-flexbox; display: flex; -ms-flex-direction: column; flex-direction: column; -ms-flex-align: center; align-items: center; -ms-flex-pack: start; justify-content: flex-start;"
  ]

demoContentTransition :: forall r w i. Array (IProp I.HTMLdiv i)
demoContentTransition =
  [ HP.style "max-width: 900px; width: 100%;"
  ]

hero :: forall r w i. Array (IProp I.HTMLdiv i)
hero =
  [ HP.style "display: -ms-flexbox; display: flex; -ms-flex-flow: row nowrap; flex-flow: row nowrap; -ms-flex-align: center; align-items: center; -ms-flex-pack: center; justify-content: center; min-height: 360px; padding: 24px; background-color: #f2f2f2;"
  ]

demoTitle :: forall r w i. Array (IProp I.HTMLh2 i)
demoTitle =
  [ HP.style "border-bottom: 1px solid rgba(0,0,0,.87);"
  ]

resourcesGraphic :: forall r w i. Array (IProp ( style :: String | r ) i)
resourcesGraphic =
  [ HP.style "width: 30px; height: 30px;"
  ]
