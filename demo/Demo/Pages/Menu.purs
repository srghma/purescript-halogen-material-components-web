module Demo.Pages.Menu where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils
import Halogen
import Material.Classes.Menu
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.List as List
import HalogenMWC.List.Divider as List.Divider
import HalogenMWC.List.Item as List.Item
import HalogenMWC.Menu as Menu
import Material.Classes.MenuSurface

type State = { open :: Boolean }

type ChildSlots = ()

type Message = Void

initialState :: forall r w i . State
initialState = { open: false }

data Action
  = Open
  | Close

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
       Open -> H.modify_ (_ { open = true })
       Close -> H.modify_ (_ { open = false })

config :: CatalogPage
config =
    { title: "Menu"
    , prelude: "Menus display a list of choices on a transient sheet of material."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-menus"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Menu"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-menu"
        }
    , hero: mkComponentStatic $
      HH.div
          [ HP.classes [ mdc_menu_surface, mdc_menu_surface____open ]
          , HP.style "position: relative; transform-origin: left top 0px; left: 0px; top: 0px; z-index: 0;"
          ]
          [ List.list List.defaultConfig
              (List.Item.listItem List.Item.defaultConfig [ HH.text "A Menu Item" ])
              [ List.Item.listItem List.Item.defaultConfig [ HH.text "Another Menu Item" ]
              ]
          ]
    , content:
        H.mkComponent
          { initialState: const initialState
          , render: \state ->
              HH.div_
              [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Anchored menu" ]
              , HH.div [ HP.class_ mdc_menu_surface____anchor ]
                  [ Button.button Button.Text (Button.defaultConfig { additionalAttributes = [ HE.onClick (const Open) ] }) [ HH.text "Open menu" ]
                  , Menu.menu
                      (Menu.defaultConfig
                          { open = state.open
                          , onClose = Just $ const $ Close
                          }
                      )
                      [ List.list (List.defaultConfig { wrapFocus = true })
                          (listItem "Passionfruit")
                          (Array.concat
                              [ map listItem
                                  [ "Orange"
                                  , "Guava"
                                  , "Pitaya"
                                  ]
                              , [ List.Divider.listItem List.Divider.defaultConfig ]
                              , map listItem
                                  [ "Pineapple"
                                  , "Mango"
                                  , "Papaya"
                                  , "Lychee"
                                  ]
                              ]
                          )
                      ]
                  ]
              ]
          , eval: H.mkEval H.defaultEval { handleAction = handleAction }
          }
    }

listItem :: forall w . String -> List.Item.ListItem w Action
listItem label = List.Item.listItem (List.Item.defaultConfig { onClick = Just $ const $ Close }) [ HH.text label ]
