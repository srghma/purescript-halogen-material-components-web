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
import HalogenMWC.Typography as Typography

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

type CatalogPageConfig topMsg =
    { openDrawer :: topMsg
    , closeDrawer :: topMsg
    , drawerOpen :: Boolean
    , route :: Route
    }

view :: (w i -> topMsg) -> CatalogPageConfig topMsg -> CatalogPage w i -> HH.HTML topMsg
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
                            # IconButton.setAttributes [ TopAppBar.navigationIcon ]
                            # IconButton.setOnClick toggleCatalogDrawer
                        )
                        "menu"
                    , HH.span
                        [ TopAppBar.title
                        , HP.style "text-transform: uppercase"
                        , HP.style "font-weight: 400"
                        ]
                        [ HH.text "Material Components for Elm" ]
                    ]
                ]
            ]
        , HH.div demoPanel
            [ DismissibleDrawer.drawer
                (DismissibleDrawer.defaultConfig
                    # DismissibleDrawer.setOpen catalogPageConfig.drawerOpen
                    # DismissibleDrawer.setAttributes
                        [ TopAppBar.fixedAdjust
                        , HP.style "z-index: 1"
                        ]
                )
                [ DismissibleDrawer.content []
                    [ case
                        map
                            (\{ route, label } ->
                                List.Item.listItem
                                    (List.Item.defaultConfig
                                        # List.Item.setSelected
                                            (if catalogPageConfig.route == route then
                                                Just List.Item.activated

                                             else
                                                Nothing
                                            )
                                        # List.Item.setHref (Just (Route.toString route))
                                    )
                                    [ HH.text label ]
                            )
                            catalogDrawerItems
                      of
                        [ listItem, listItems ] ->
                            Array.list Array.defaultConfig listItem listItems

                        _ ->
                            text ""
                    ]
                ]
            , HH.map lift $
                HH.div
                    [ TopAppBar.fixedAdjust, DismissibleDrawer.appContent, demoContent ]
                    [ HH.div demoContentTransition
                        [ HH.h1 [ Typography.headline5 ] [ HH.text catalogPage.title ]
                        , HH.p [ Typography.body1 ] [ HH.text catalogPage.prelude ]
                        , HH.div hero catalogPage.hero
                        , HH.h2 [ Typography.headline6, demoTitle ] [ HH.text "Resources" ]
                        , resourcesList catalogPage.resources
                        , HH.h2 [ Typography.headline6, demoTitle ] [ HH.text "Demos" ]
                        , catalogPage.content
                        ]
                    ]
            ]
        ]

resourcesList :: CatalogPageResources -> HH.HTML w i
resourcesList { materialDesignGuidelines, documentation, sourceCode } =
    Array.list Array.defaultConfig
        (List.Item.listItem
            (List.Item.defaultConfig
                # List.Item.setHref materialDesignGuidelines
                # List.Item.setTarget (Just "_blank")
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "images/ic_material_design_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Material Design Guidelines"
            ]
        )
        [ List.Item.listItem
            (List.Item.defaultConfig
                # List.Item.setHref documentation
                # List.Item.setTarget (Just "_blank")
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "images/ic_drive_document_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Documentation"
            ]
        , List.Item.listItem
            (List.Item.defaultConfig
                # List.Item.setHref sourceCode
                # List.Item.setTarget (Just "_blank")
            )
            [ List.Item.graphic resourcesGraphic
                [ HH.img
                    (HP.src "images/ic_code_24px.svg"
                        :: resourcesGraphic
                    )
                    []
                ]
            , HH.text "Source Code"
            ]
        ]

catalogDrawerItems :: Array { label :: String, route :: Route }
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

catalogPageContainer :: Array (HH.Attribute w i)
catalogPageContainer =
    [ HP.style "position: relative"
    , Typography.typography
    ]

demoPanel :: Array (HH.Attribute w i)
demoPanel =
    [ HP.style "display: -ms-flexbox"
    , HP.style "display: flex"
    , HP.style "position: relative"
    , HP.style "height: 100vh"
    , HP.style "overflow: hidden"
    ]

demoContent :: Array (HH.Attribute w i)
demoContent =
    [ HP.id "demo-content"
    , HP.style "height: 100%"
    , HP.style "-webkit-box-sizing: border-box"
    , HP.style "box-sizing: border-box"
    , HP.style "max-width: 100%"
    , HP.style "padding-left: 16px"
    , HP.style "padding-right: 16px"
    , HP.style "padding-bottom: 100px"
    , HP.style "width: 100%"
    , HP.style "overflow: auto"
    , HP.style "display: -ms-flexbox"
    , HP.style "display: flex"
    , HP.style "-ms-flex-direction: column"
    , HP.style "flex-direction: column"
    , HP.style "-ms-flex-align: center"
    , HP.style "align-items: center"
    , HP.style "-ms-flex-pack: start"
    , HP.style "justify-content: flex-start"
    ]

demoContentTransition :: Array (HH.Attribute w i)
demoContentTransition =
    [ HP.style "max-width: 900px"
    , HP.style "width: 100%"
    ]

hero :: Array (HH.Attribute w i)
hero =
    [ HP.style "display: -ms-flexbox"
    , HP.style "display: flex"
    , HP.style "-ms-flex-flow: row nowrap"
    , HP.style "flex-flow: row nowrap"
    , HP.style "-ms-flex-align: center"
    , HP.style "align-items: center"
    , HP.style "-ms-flex-pack: center"
    , HP.style "justify-content: center"
    , HP.style "min-height: 360px"
    , HP.style "padding: 24px"
    , HP.style "background-color: #f2f2f2"
    ]

demoTitle :: Array (HH.Attribute w i)
demoTitle =
    [ HP.style "border-bottom: 1px solid rgba(0,0,0,.87)"
    ]

resourcesGraphic :: Array (HH.Attribute w i)
resourcesGraphic =
    [ HP.style "width: 30px"
    , HP.style "height: 30px"
    ]
