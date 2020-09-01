module Demo.Pages.DrawerPage where

import Halogen
import Material.Classes.Drawer
import Material.Classes.TopAppBar
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Drawer.Dismissible as Drawer.Dismissible
import HalogenMWC.Drawer.Permanent as Drawer.Permanent
import HalogenMWC.Icon as Icon
import HalogenMWC.List as List
import HalogenMWC.List.Divider as List.Divider
import HalogenMWC.List.Item as List.Item
import HalogenMWC.TopAppBar as TopAppBar
import Web.UIEvent.MouseEvent (MouseEvent)

type DrawerPage w i =
  { title :: String
  , drawer :: HH.HTML w i
  , scrim :: Maybe (HH.HTML w i)
  , onMenuClick :: Maybe (MouseEvent -> i)
  }

render :: forall w i . DrawerPage w i -> HH.HTML w i
render page =
  HH.div drawerFrameRoot
    [ page.drawer
    , Maybe.fromMaybe (HH.text "") page.scrim
    , HH.div [ HP.class_ mdc_drawer_app_content ]
        [ TopAppBar.topAppBar TopAppBar.defaultConfig
            [ HH.section [ HP.class_ mdc_top_app_bar__row ]
                [ HH.section [ HP.classes [ mdc_top_app_bar__section, mdc_top_app_bar__section____align_start ] ]
                    [ case page.onMenuClick of
                        Just handleClick ->
                            Icon.materialIcon
                              [ HP.class_ mdc_top_app_bar__navigation_icon
                              , HE.onClick handleClick
                              ]
                              "menu"

                        Nothing -> HH.text ""
                    , HH.span [ HP.class_ mdc_top_app_bar__title ] [ HH.text page.title ]
                    ]
                ]
            ]
        , mainContent
        ]
    ]

drawerBody :: forall r w i . (Int -> i) -> Int -> Array (HH.HTML w i)
drawerBody setSelectedIndex selectedIndex =
    let
      listItemConfig index =
        List.Item.defaultConfig
          { selected =
              (if selectedIndex == index then
                  Just List.Item.Activated

                else
                  Nothing
              )
          , onClick = Just $ const $ setSelectedIndex index
          }
    in
    [ Drawer.Permanent.header
        [ HH.h3 [ HP.class_ mdc_drawer__title ] [ HH.text "Mail" ]
        , HH.h6 [ HP.class_ mdc_drawer__subtitle ] [ HH.text "email@material.io" ]
        ]
    , Drawer.Permanent.content
        [ List.group
            [ List.list List.defaultConfig
                (List.Item.listItem (listItemConfig 0)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "inbox" ]
                    , HH.text "Inbox"
                    ]
                )
                [ List.Item.listItem (listItemConfig 1)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "star" ]
                    , HH.text "Star"
                    ]
                , List.Item.listItem (listItemConfig 2)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "send" ]
                    , HH.text "Sent Mail"
                    ]
                , List.Item.listItem (listItemConfig 3)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "drafts" ]
                    , HH.text "Drafts"
                    ]
                ]
            , List.Divider.listDivider
            , List.subheader [ HH.text "Labels" ]
            , List.list List.defaultConfig
                (List.Item.listItem (listItemConfig 4)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "bookmark" ]
                    , HH.text "Family"
                    ]
                )
                [ List.Item.listItem (listItemConfig 5)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "bookmark" ]
                    , HH.text "Friends"
                    ]
                , List.Item.listItem (listItemConfig 6)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "bookmark" ]
                    , HH.text "Work"
                    ]
                , List.Divider.listItem List.Divider.defaultConfig
                , List.Item.listItem (listItemConfig 7)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "settings" ]
                    , HH.text "Settings"
                    ]
                , List.Item.listItem (listItemConfig 8)
                    [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] "announcement" ]
                    , HH.text "Help & feedback"
                    ]
                ]
            ]
        ]
    ]

mainContent :: forall w i . HH.HTML w i
mainContent =
    HH.div
      [ HP.style "padding-left: 18px; padding-right: 18px; overflow: auto; height: 100%; box-sizing: border-box;"
      , HP.classes [ mdc_top_app_bar____fixed_adjust, mdc_drawer_app_content ]
      ]
      (Array.replicate 4 $ HH.p [] [ HH.text loremIpsum ])

loremIpsum :: String
loremIpsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

drawerFrameRoot :: forall r w i . Array (IProp I.HTMLdiv i)
drawerFrameRoot =
    [ HP.style "display: -ms-flexbox; display: flex; height: 100vh;"
    ]
