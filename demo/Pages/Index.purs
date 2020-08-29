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
import Demo.Route
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

----------
-- HTML
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
                { additionalClasses: [ mdc_top_app_bar__navigation_icon ]
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
          { additionaladditionalAttributes =
            [ HP.style "max-width: 900px; padding-top: 128px; padding-bottom: 100px;"
            ]
          }
        )
        (map
            (\{ route, title, icon } ->
                ImageList.Item.ImageListItem
                ( { label: Just title
                  , href: Just ("#" <> Routing.Duplex.print routeCodec route)
                  , image: "https://aforemny.github.io/material-components-web-elm/" <> icon
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
  [ { route: Button
    , icon: "images/buttons_180px.svg"
    , title: "Button"
    , subtitle: "Raised and flat buttons"
    }
  -- | , { route: Card
  -- |   , icon: "images/cards_180px.svg"
  -- |   , title: "Card"
  -- |   , subtitle: "Various card layout styles"
  -- |   }
  -- | , { route: Checkbox
  -- |   , icon: "images/checkbox_180px.svg"
  -- |   , title: "Checkbox"
  -- |   , subtitle: "Multi-selection controls"
  -- |   }
  -- | , { route: Chips
  -- |   , icon: "images/chips_180px.svg"
  -- |   , title: "Chips"
  -- |   , subtitle: "Chips"
  -- |   }
  -- | , { route: DataTable
  -- |   , icon: "images/data_table_180px.svg"
  -- |   , title: "Data Table"
  -- |   , subtitle: "Data Table"
  -- |   }
  -- | , { route: Dialog
  -- |   , icon: "images/dialog_180px.svg"
  -- |   , title: "Dialog"
  -- |   , subtitle: "Secondary text"
  -- |   }
  -- | , { route: Drawer
  -- |   , icon: "images/drawer_180px.svg"
  -- |   , title: "Drawer"
  -- |   , subtitle: "Various drawer styles"
  -- |   }
  -- | , { route: Elevation
  -- |   , icon: "images/elevation_180px.svg"
  -- |   , title: "Elevation"
  -- |   , subtitle: "Shadow for different elevations"
  -- |   }
  -- | , { route: Fab
  -- |   , icon: "images/floating_action_button_180px.svg"
  -- |   , title: "FAB"
  -- |   , subtitle: "The primary action in an application"
  -- |   }
  -- | , { route: IconButton
  -- |   , icon: "images/icon_button_180px.svg"
  -- |   , title: "Icon Button"
  -- |   , subtitle: "Toggling icon states"
  -- |   }
  -- | , { route: ImageList
  -- |   , icon: "images/image_list_180px.svg"
  -- |   , title: "Image List"
  -- |   , subtitle: "An Image List consists of several items, each containing an image and optionally supporting content (i.e. a text label)"
  -- |   }
  -- | , { route: LayoutGrid
  -- |   , icon: "images/layout_grid_180px.svg"
  -- |   , title: "Layout Grid"
  -- |   , subtitle: "Grid and gutter support"
  -- |   }
  -- | , { route: List
  -- |   , icon: "images/list_180px.svg"
  -- |   , title: "List"
  -- |   , subtitle: "Item layouts in lists"
  -- |   }
  -- | , { route: LinearProgress
  -- |   , icon: "images/linear_progress_180px.svg"
  -- |   , title: "Linear progress"
  -- |   , subtitle: "Fills from 0% to 100%, represented by bars"
  -- |   }
  -- | , { route: Menu
  -- |   , icon: "images/menu_180px.svg"
  -- |   , title: "Menu"
  -- |   , subtitle: "Pop over menus"
  -- |   }
  -- | , { route: RadioButton
  -- |   , icon: "images/radio_180px.svg"
  -- |   , title: "Radio"
  -- |   , subtitle: "Single selection controls"
  -- |   }
  -- | , { route: Ripple
  -- |   , icon: "images/ripple_180px.svg"
  -- |   , title: "Ripple"
  -- |   , subtitle: "Touch ripple"
  -- |   }
  -- | , { route: Select
  -- |   , icon: "images/form_field_180px.svg"
  -- |   , title: "Select"
  -- |   , subtitle: "Popover selection menus"
  -- |   }
  -- | , { route: Slider
  -- |   , icon: "images/slider_180px.svg"
  -- |   , title: "Slider"
  -- |   , subtitle: "Range Controls"
  -- |   }
  -- | , { route: Snackbar
  -- |   , icon: "images/snackbar_180px.svg"
  -- |   , title: "Snackbar"
  -- |   , subtitle: "Transient messages"
  -- |   }
  -- | , { route: Switch
  -- |   , icon: "images/switch_180px.svg"
  -- |   , title: "Switch"
  -- |   , subtitle: "On off switches"
  -- |   }
  -- | , { route: TabBar
  -- |   , icon: "images/tabs_180px.svg"
  -- |   , title: "Tab Bar"
  -- |   , subtitle: "Tabs organize and allow navigation between groups of content that are related and at the same level of hierarchy"
  -- |   }
  -- | , { route: TextField
  -- |   , icon: "images/form_field_180px.svg"
  -- |   , title: "Text field"
  -- |   , subtitle: "Single and multiline text fields"
  -- |   }
  -- | , { route: Theme
  -- |   , icon: "images/ic_theme_24px.svg"
  -- |   , title: "Theme"
  -- |   , subtitle: "Using primary and accent colors"
  -- |   }
  -- | , { route: TopAppBar
  -- |   , icon: "images/top_app_bar_180px.svg"
  -- |   , title: "Top App Bar"
  -- |   , subtitle: "Container for items such as application title, navigation icon, and action items."
  -- |   }
  -- | , { route: Typography
  -- |   , icon: "images/fonts_180px.svg"
  -- |   , title: "Typography"
  -- |   , subtitle: "Type hierarchy"
  -- |   }
  ]
