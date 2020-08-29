module Demo.Pages.Index where

import Protolude

import Material.Classes.TopAppBar
import Data.Const (Const(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.IconButton as IconButton
import HalogenMWC.ImageList as ImageList
import HalogenMWC.ImageList.Item as ImageList.Item
import HalogenMWC.TopAppBar as TopAppBar
import Demo.Route (Route)
import Demo.Route as Route
import Routing.Duplex as Routing.Duplex

type State
  = Unit

type Query
  = Const Void

type Action
  = Unit

type Input
  = Unit

type Message
  = Void

component ::
  ∀ m. H.Component Query Input Message m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: forall r w i . State -> H.ComponentHTML Action () m
  render _ =
    HH.div_
    [ TopAppBar.topAppBar TopAppBar.defaultConfig
      [ HH.section
        [ HP.class_ mdc_top_app_bar__row
        ]
        [ HH.section
          [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ]
          ]
          [ IconButton.iconButton
              ( IconButton.defaultConfig
                { additionalClasses = [ mdc_top_app_bar__navigation_icon ]
                }
              )
              [ HH.img
                  [ HP.src "https://material-components-web.appspot.com/images/ic_component_24px_white.svg"
                  ]
              ]
          , HH.span
              [ HP.class_ mdc_top_app_bar__title
              , HP.style "text-transform: uppercase; font-weight: 400;"
              ]
              [ HH.text "Material Components for Halogen" ]
          ]
        ]
      ]
    , ImageList.imageList
        (ImageList.defaultConfig
          { additionalAttributes =
            [ HP.style "max-width: 900px; padding-top: 128px; padding-bottom: 100px;"
            ]
          }
        )
        (map
            (\{ route, title, icon } ->
                ImageList.Item.ImageListItem
                ( { label: Just title
                  , href: Just (Route.toString route)
                  , image: icon
                  , additionalAttributes:
                    [ HP.style "width: calc(100% / 4 - 8.25px); margin: 4px;"
                    ]
                  }
                )
            )
            imageListItems
        )
    ]

imageListItems :: forall r w i . Array { route :: Route, icon :: String, title :: String, subtitle :: String }
imageListItems =
  [ { route: Route.Buttons
    , icon: "https://aforemny.github.io/material-components-web-elm/images/buttons_180px.svg"
    , title: "Button"
    , subtitle: "Raised and flat buttons"
    }
  -- | , { route: Card
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/cards_180px.svg"
  -- |   , title: "Card"
  -- |   , subtitle: "Various card layout styles"
  -- |   }
  -- | , { route: Checkbox
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/checkbox_180px.svg"
  -- |   , title: "Checkbox"
  -- |   , subtitle: "Multi-selection controls"
  -- |   }
  -- | , { route: Chips
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/chips_180px.svg"
  -- |   , title: "Chips"
  -- |   , subtitle: "Chips"
  -- |   }
  -- | , { route: DataTable
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/data_table_180px.svg"
  -- |   , title: "Data Table"
  -- |   , subtitle: "Data Table"
  -- |   }
  -- | , { route: Dialog
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/dialog_180px.svg"
  -- |   , title: "Dialog"
  -- |   , subtitle: "Secondary text"
  -- |   }
  -- | , { route: Drawer
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/drawer_180px.svg"
  -- |   , title: "Drawer"
  -- |   , subtitle: "Various drawer styles"
  -- |   }
  -- | , { route: Elevation
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/elevation_180px.svg"
  -- |   , title: "Elevation"
  -- |   , subtitle: "Shadow for different elevations"
  -- |   }
  -- | , { route: Fab
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/floating_action_button_180px.svg"
  -- |   , title: "FAB"
  -- |   , subtitle: "The primary action in an application"
  -- |   }
  -- | , { route: IconButton
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/icon_button_180px.svg"
  -- |   , title: "Icon Button"
  -- |   , subtitle: "Toggling icon states"
  -- |   }
  -- | , { route: ImageList
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/image_list_180px.svg"
  -- |   , title: "Image List"
  -- |   , subtitle: "An Image List consists of several items, each containing an image and optionally supporting content (i.e. a text label)"
  -- |   }
  -- | , { route: LayoutGrid
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/layout_grid_180px.svg"
  -- |   , title: "Layout Grid"
  -- |   , subtitle: "Grid and gutter support"
  -- |   }
  -- | , { route: List
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/list_180px.svg"
  -- |   , title: "List"
  -- |   , subtitle: "Item layouts in lists"
  -- |   }
  -- | , { route: LinearProgress
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/linear_progress_180px.svg"
  -- |   , title: "Linear progress"
  -- |   , subtitle: "Fills from 0% to 100%, represented by bars"
  -- |   }
  -- | , { route: Menu
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/menu_180px.svg"
  -- |   , title: "Menu"
  -- |   , subtitle: "Pop over menus"
  -- |   }
  -- | , { route: RadioButton
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/radio_180px.svg"
  -- |   , title: "Radio"
  -- |   , subtitle: "Single selection controls"
  -- |   }
  -- | , { route: Ripple
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/ripple_180px.svg"
  -- |   , title: "Ripple"
  -- |   , subtitle: "Touch ripple"
  -- |   }
  -- | , { route: Select
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/form_field_180px.svg"
  -- |   , title: "Select"
  -- |   , subtitle: "Popover selection menus"
  -- |   }
  -- | , { route: Slider
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/slider_180px.svg"
  -- |   , title: "Slider"
  -- |   , subtitle: "Range Controls"
  -- |   }
  -- | , { route: Snackbar
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/snackbar_180px.svg"
  -- |   , title: "Snackbar"
  -- |   , subtitle: "Transient messages"
  -- |   }
  -- | , { route: Switch
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/switch_180px.svg"
  -- |   , title: "Switch"
  -- |   , subtitle: "On off switches"
  -- |   }
  -- | , { route: TabBar
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/tabs_180px.svg"
  -- |   , title: "Tab Bar"
  -- |   , subtitle: "Tabs organize and allow navigation between groups of content that are related and at the same level of hierarchy"
  -- |   }
  -- | , { route: TextField
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/form_field_180px.svg"
  -- |   , title: "Text field"
  -- |   , subtitle: "Single and multiline text fields"
  -- |   }
  -- | , { route: Theme
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/ic_theme_24px.svg"
  -- |   , title: "Theme"
  -- |   , subtitle: "Using primary and accent colors"
  -- |   }
  -- | , { route: TopAppBar
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/top_app_bar_180px.svg"
  -- |   , title: "Top App Bar"
  -- |   , subtitle: "Container for items such as application title, navigation icon, and action items."
  -- |   }
  -- | , { route: Typography
  -- |   , icon: "https://aforemny.github.io/material-components-web-elm/images/fonts_180px.svg"
  -- |   , title: "Typography"
  -- |   , subtitle: "Type hierarchy"
  -- |   }
  ]
