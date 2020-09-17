module Demo.Pages.TabBar where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HalogenMWC.Button as Button
import HalogenMWC.Tab as Tab
import HalogenMWC.TabBar as TabBar
import Material.Classes.Typography (mdc_typography____subtitle1)
import Demo.Utils (focusById)

type ChildSlots = ()

type Message = Void

type State =
  { activeHeroTab :: Int
  , activeIconTab :: Int
  , activeStackedTab :: Int
  , activeScrollingTab :: Int
  }

initialState :: forall r w i . State
initialState =
  { activeHeroTab: 0
  , activeIconTab: 0
  , activeStackedTab: 0
  , activeScrollingTab: 0
  }

data Action
  = SetActiveHeroTab Int
  | SetActiveIconTab Int
  | SetActiveStackedTab Int
  | SetActiveScrollingTab Int
  | Focus String

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
    case _ of
        SetActiveHeroTab index      -> H.modify_ (_ { activeHeroTab = index })
        SetActiveIconTab index      -> H.modify_ (_ { activeIconTab = index })
        SetActiveStackedTab index   -> H.modify_ (_ { activeStackedTab = index })
        SetActiveScrollingTab index -> H.modify_ (_ { activeScrollingTab = index })
        Focus id                    -> H.liftEffect $ focusById id

config :: CatalogPage
config =
  { title: "Tab Bar"
  , prelude: "Tabs organize and allow navigation between groups of content that are related and at the same level of hierarchy. The Tab Bar contains the Tab Scroller and Tab components."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-tabs"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-TabBar"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-tab-bar"
      }
  , hero:
      H.mkComponent
        { initialState: const initialState
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        , render: \state ->
          TabBar.tabBar TabBar.defaultConfig
            [ (Tab.defaultConfig { label: "Home", icon: Nothing })
                { active = state.activeHeroTab == 0
                , onClick = Just $ const $ SetActiveHeroTab 0
                }
            , (Tab.defaultConfig { label: "Merchandise", icon: Nothing })
                { active = state.activeHeroTab == 1
                , onClick = Just $ const $ SetActiveHeroTab 1
                }
            , (Tab.defaultConfig { label: "About Us", icon: Nothing })
                { active = (state.activeHeroTab == 2)
                , onClick = Just $ const $ SetActiveHeroTab 2
                }
            ]
        }
  , content:
      H.mkComponent
        { initialState: const initialState
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        , render: \state ->
            HH.div_
              [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Tabs with icons next to labels" ]
              , tabsWithIcons state
              , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Tabs with icons above labels and indicators restricted to content" ]
              , tabsWithStackedIcons state
              , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Scrolling tabs" ]
              , scrollingTabs state
              , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus tabs" ]
              , focusTabs state
              ]
        }
  }

tabsWithIcons :: forall w . State -> HH.HTML w Action
tabsWithIcons state =
  let
    config index content =
      (Tab.defaultConfig content)
        { active = state.activeIconTab == index
        , onClick = Just $ const $ SetActiveIconTab index
        }
  in
  TabBar.tabBar TabBar.defaultConfig
    [ config 0 { label: "Recents", icon: Just "access_time" }
    , config 1 { label: "Nearby", icon: Just "near_me" }
    , config 2 { label: "Favorites", icon: Just "favorite" }
    ]

tabsWithStackedIcons :: forall w . State -> HH.HTML w Action
tabsWithStackedIcons state =
  let
    config index content =
      (Tab.defaultConfig content)
        { active = state.activeStackedTab == index
        , onClick = Just $ const $ SetActiveStackedTab index
        }
  in
  TabBar.tabBar
    (TabBar.defaultConfig
        { stacked = true
        , indicatorSpansContent = true
        }
    )
    [ config 0 { label: "Recents", icon: Just "access_time" }
    , config 1 { label: "Nearby", icon: Just "near_me" }
    , config 2 { label: "Favorites", icon: Just "favorite" }
    ]

scrollingTabs :: forall w . State -> HH.HTML w Action
scrollingTabs state =
  let
    config index content =
      (Tab.defaultConfig content)
        { active = state.activeScrollingTab == index
        , onClick = Just $ const $ SetActiveScrollingTab index
        }
  in
  TabBar.tabBar TabBar.defaultConfig
    (Array.mapWithIndex
      (\index label -> config index { label: "Tab " <> label, icon: Nothing })
      [ "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight" ]
    )

focusTabs :: forall w . State -> HH.HTML w Action
focusTabs state =
  HH.div_
    [ TabBar.tabBar
        (TabBar.defaultConfig
            { additionalAttributes = [ HP.id_ "my-tabs" ]
            }
        )
        [ (Tab.defaultConfig { label: "Home", icon: Nothing })
            { active = state.activeHeroTab == 0
            , onClick = Just $ const $ SetActiveHeroTab 0
            }
        , (Tab.defaultConfig { label: "Merchandise", icon: Nothing })
            { active = state.activeHeroTab == 1
            , onClick = Just $ const $ SetActiveHeroTab 1
            }
        , (Tab.defaultConfig { label: "About Us", icon: Nothing })
            { active = state.activeHeroTab == 2
            , onClick = Just $ const $ SetActiveHeroTab 2
            }
        ]
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-tabs") ] })
        [ HH.text "Focus" ]
    ]
