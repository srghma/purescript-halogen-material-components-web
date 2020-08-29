module Demo.HOC.CatalogPage where

import Demo.Route as Route
import Demo.Route (Route)
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Drawer.Dismissible as DismissibleDrawer
import HalogenMWC.IconButton as IconButton
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import HalogenMWC.TopAppBar as TopAppBar
import Material.Classes.Typography

type CatalogPage w i =
    { title :: String
    , prelude :: String
    , resources :: CatalogPageResources
    , hero :: Array (HH.HTML w i)
    , content :: Array (HH.HTML w i)
    }

type CatalogPageResources =
    { materialDesignGuidelines :: Maybe String
    , documentation :: Maybe String
    , sourceCode :: Maybe String
    }

type CatalogPageConfig topAction =
    { openDrawer :: topAction
    , closeDrawer :: topAction
    , drawerOpen :: Boolean
    , route :: Route
    }

view :: forall r w i . (w i -> topAction) -> CatalogPageConfig topAction -> CatalogPage w i -> HH.HTML topAction
view lift catalogPageConfig catalogPage =
    let
        toggleCatalogDrawer =
            if catalogPageConfig.drawerOpen then
                catalogPageConfig.closeDrawer

            else
                catalogPageConfig.openDrawer
    in
    HH.div catalogPageContainer
        [ TopAppBar.regular TopAppBar.defaultConfig
            [ TopAppBar.row []
                [ TopAppBar.section [ TopAppBar.alignStart ]
                    [ IconButton.iconButton
                        (IconButton.defaultConfig
                            { additionalAttributes = [ TopAppBar.navigationIcon ]
                            , onClick = toggleCatalogDrawer
                        )
                        "menu"
                    , HH.span
                        [ TopAppBar.title
                        , HP.style "text-transform: uppercase; font-weight: 400;"
                        ]
                        [ HH.text "Material Components for Elm" ]
                    ]
                ]
            ]
        , HH.div demoPanel
            [ DismissibleDrawer.drawer
                (DismissibleDrawer.defaultConfig
                    { open = catalogPageConfig.drawerOpen
                    , additionalAttributes =
                        [ TopAppBar.fixedAdjust
                        , HP.style "z-index: 1;"
                        ]
                )
                [ DismissibleDrawer.content []
                    [ case
                        map
                            (\{ route, label } ->
                                List.Item.listItem
                                    (List.Item.defaultConfig
                                        { selected =
                                            (if catalogPageConfig.route == route then
                                                Just List.Item.activated

                                             else
                                                Nothing
                                            )
                                        , href = (Just (Route.toString route))
                                    )
                                    [ HH.text label ]
                            )
                            catalogDrawerItems
                      of
                        [ listItem, listItems ] ->
                            List.list Array.defaultConfig listItem listItems

                        _ ->
                            text ""
                    ]
                ]
            , HH.map lift $
                HH.div
                    [ TopAppBar.fixedAdjust, DismissibleDrawer.appContent, demoContent ]
                    [ HH.div demoContentTransition
                        [ HH.h1 [ mdc_typography____headline5 ] [ HH.text catalogPage.title ]
                        , HH.p [ mdc_typography____body1 ] [ HH.text catalogPage.prelude ]
                        , HH.div hero catalogPage.hero
                        , HH.h2 [ mdc_typography____headline6, demoTitle ] [ HH.text "Resources" ]
                        , resourcesList catalogPage.resources
                        , HH.h2 [ mdc_typography____headline6, demoTitle ] [ HH.text "Demos" ]
                        , catalogPage.content
                        ]
                    ]
            ]
        ]

resourcesList :: forall r w i . CatalogPageResources -> HH.HTML w i
resourcesList { materialDesignGuidelines, documentation, sourceCode } =
    List.list Array.defaultConfig
        (List.Item.listItem
            ( List.Item.defaultConfig
                { href = materialDesignGuidelines
                , target = Just "_blank"
                }
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_material_design_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Material Design Guidelines"
            ]
        )
        [ List.Item.listItem
            (List.Item.defaultConfig
                { href = documentation
                , target = (Just "_blank")
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_drive_document_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Documentation"
            ]
        , List.Item.listItem
            (List.Item.defaultConfig
                { href = sourceCode
                , target = (Just "_blank")
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "https://aforemny.github.io/material-components-web-elm/images/ic_code_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Source Code"
            ]
        ]

catalogDrawerItems :: forall r w i . Array { label :: String, route :: Route }
catalogDrawerItems =
    [ { label: "Home", route: Route.StartPage }
    , { label: "Button", route: Route.Button }
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

catalogPageContainer :: forall r w i . Array (IProp r i)
catalogPageContainer =
    [ HP.style "position: relative;"
    , mdc_typography____typography
    ]

demoPanel :: forall r w i . Array (IProp r i)
demoPanel =
    [ HP.style "display: -ms-flexbox; display: flex; position: relative; height: 100vh; overflow: hidden;"
    ]

demoContent :: forall r w i . Array (IProp r i)
demoContent =
    [ HP.id "demo-content"
    , HP.style "height: 100%; -webkit-box-sizing: border-box; box-sizing: border-box; max-width: 100%; padding-left: 16px; padding-right: 16px; padding-bottom: 100px; width: 100%; overflow: auto; display: -ms-flexbox; display: flex; -ms-flex-direction: column; flex-direction: column; -ms-flex-align: center; align-items: center; -ms-flex-pack: start; justify-content: flex-start;"
    ]

demoContentTransition :: forall r w i . Array (IProp r i)
demoContentTransition =
    [ HP.style "max-width: 900px; width: 100%;"
    ]

hero :: forall r w i . Array (IProp r i)
hero =
    [ HP.style "display: -ms-flexbox; display: flex; -ms-flex-flow: row nowrap; flex-flow: row nowrap; -ms-flex-align: center; align-items: center; -ms-flex-pack: center; justify-content: center; min-height: 360px; padding: 24px; background-color: #f2f2f2;"
    ]

demoTitle :: forall r w i . Array (IProp r i)
demoTitle =
    [ HP.style "border-bottom: 1px solid rgba(0,0,0,.87);"
    ]

resourcesGraphic :: forall r w i . Array (IProp r i)
resourcesGraphic =
    [ HP.style "width: 30px; height: 30px;"
    ]
