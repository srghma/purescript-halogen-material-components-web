module Demo.HOC.CatalogPage where

import Demo.Route
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

data CatalogPage w i =
    { title :: String
    , prelude :: String
    , resources :: CatalogPageResources
    , hero :: Array (HH.HTML w i)
    , content :: Array (HH.HTML w i)
    }

data CatalogPageResources =
    { materialDesignGuidelines :: Maybe String
    , documentation :: Maybe String
    , sourceCode :: Maybe String
    }

data CatalogPageConfig topMsg =
    { openDrawer :: topMsg
    , closeDrawer :: topMsg
    , drawerOpen :: Boolean
    , url :: Url
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
                        , style "text-transform" "uppercase"
                        , style "font-weight" "400"
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
                        , style "z-index" "1"
                        ]
                )
                [ DismissibleDrawer.content []
                    [ case
                        map
                            (\{ url, label } ->
                                List.Item.listItem
                                    (List.Item.defaultConfig
                                        # List.Item.setSelected
                                            (if catalogPageConfig.url == url then
                                                Just List.Item.activated

                                             else
                                                Nothing
                                            )
                                        # List.Item.setHref (Just (Url.toString url))
                                    )
                                    [ HH.text label ]
                            )
                            catalogDrawerItems
                      of
                        listItem :: listItems ->
                            Array.list Array.defaultConfig listItem listItems

                        [] ->
                            text ""
                    ]
                ]
            , HH.map lift $
                HH.div
                    (TopAppBar.fixedAdjust
                        :: DismissibleDrawer.appContent
                        :: demoContent
                    )
                    [ HH.div demoContentTransition
                        (HH.h1 [ Typography.headline5 ] [ HH.text catalogPage.title ]
                            :: HH.p [ Typography.body1 ] [ HH.text catalogPage.prelude ]
                            :: HH.div hero catalogPage.hero
                            :: HH.h2 (Typography.headline6 :: demoTitle)
                                [ HH.text "Resources" ]
                            :: resourcesList catalogPage.resources
                            :: HH.h2 (Typography.headline6 :: demoTitle)
                                [ HH.text "Demos" ]
                            :: catalogPage.content
                        )
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

catalogDrawerItems :: Array { label :: String, url :: Url }
catalogDrawerItems =
    [ { label = "Home", url = Url.StartPage }
    , { label = "Button", url = Url.Button }
    , { label = "Card", url = Url.Card }
    , { label = "Checkbox", url = Url.Checkbox }
    , { label = "Chips", url = Url.Chips }
    , { label = "DataTable", url = Url.DataTable }
    , { label = "Dialog", url = Url.Dialog }
    , { label = "Drawer", url = Url.Drawer }
    , { label = "Elevation", url = Url.Elevation }
    , { label = "FAB", url = Url.Fab }
    , { label = "Icon Button", url = Url.IconButton }
    , { label = "Image List", url = Url.ImageList }
    , { label = "Layout Grid", url = Url.LayoutGrid }
    , { label = "Linear Progress Indicator", url = Url.LinearProgress }
    , { label = "List", url = Url.List }
    , { label = "Menu", url = Url.Menu }
    , { label = "Radio Button", url = Url.RadioButton }
    , { label = "Ripple", url = Url.Ripple }
    , { label = "Select", url = Url.Select }
    , { label = "Slider", url = Url.Slider }
    , { label = "Snackbar", url = Url.Snackbar }
    , { label = "Switch", url = Url.Switch }
    , { label = "Tab Bar", url = Url.TabBar }
    , { label = "Text Field", url = Url.TextField }
    , { label = "Theme", url = Url.Theme }
    , { label = "Top App Bar", url = Url.TopAppBar }
    , { label = "Typography", url = Url.Typography }
    ]

catalogPageContainer :: Array (HH.Attribute w i)
catalogPageContainer =
    [ style "position" "relative"
    , Typography.typography
    ]

demoPanel :: Array (HH.Attribute w i)
demoPanel =
    [ style "display" "-ms-flexbox"
    , style "display" "flex"
    , style "position" "relative"
    , style "height" "100vh"
    , style "overflow" "hidden"
    ]

demoContent :: Array (HH.Attribute w i)
demoContent =
    [ HP.id "demo-content"
    , style "height" "100%"
    , style "-webkit-box-sizing" "border-box"
    , style "box-sizing" "border-box"
    , style "max-width" "100%"
    , style "padding-left" "16px"
    , style "padding-right" "16px"
    , style "padding-bottom" "100px"
    , style "width" "100%"
    , style "overflow" "auto"
    , style "display" "-ms-flexbox"
    , style "display" "flex"
    , style "-ms-flex-direction" "column"
    , style "flex-direction" "column"
    , style "-ms-flex-align" "center"
    , style "align-items" "center"
    , style "-ms-flex-pack" "start"
    , style "justify-content" "flex-start"
    ]

demoContentTransition :: Array (HH.Attribute w i)
demoContentTransition =
    [ style "max-width" "900px"
    , style "width" "100%"
    ]

hero :: Array (HH.Attribute w i)
hero =
    [ style "display" "-ms-flexbox"
    , style "display" "flex"
    , style "-ms-flex-flow" "row nowrap"
    , style "flex-flow" "row nowrap"
    , style "-ms-flex-align" "center"
    , style "align-items" "center"
    , style "-ms-flex-pack" "center"
    , style "justify-content" "center"
    , style "min-height" "360px"
    , style "padding" "24px"
    , style "background-color" "#f2f2f2"
    ]

demoTitle :: Array (HH.Attribute w i)
demoTitle =
    [ style "border-bottom" "1px solid rgba(0,0,0,.87)"
    ]

resourcesGraphic :: Array (HH.Attribute w i)
resourcesGraphic =
    [ style "width" "30px"
    , style "height" "30px"
    ]
