module HalogenMWC.TabBar where

import Halogen
import Material.Classes.TabBar
import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Tab as Tab
import Material.Classes.Tab
import Material.Classes.TabBar (mdc_tab_bar)
import Material.Classes.TabScroller
import Material.Classes.TabIndicator

type Config i =
  { stacked :: Boolean
  , minWidth :: Boolean
  , indicatorSpansContent :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , align :: Maybe Align
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { stacked: false
  , minWidth: false
  , indicatorSpansContent: false
  , align: Nothing
  , additionalAttributes: []
  }

tabBar :: forall i w . Config i -> Array (Tab.Config i) -> HH.HTML w i
tabBar config tabs =
  HH.element (ElemName "mdc-tab-bar")
    ( Array.catMaybes
        [ Just $ HP.class_ mdc_tab_bar
        , Just (HP.attr (AttrName "role") "tablist")
        , activeTabIndexProp tabs
        ]
        <> config.additionalAttributes
    )
    [ tabScroller config config.align tabs ]

activeTabIndexProp :: forall i r . Array (Tab.Config i) -> Maybe (IProp r i)
activeTabIndexProp tabs =
  let
    activeTabIndex :: Maybe Int
    activeTabIndex = Array.findIndex (\tabConfig -> tabConfig.active) tabs
  in
    map (HP.prop (PropName "activeTabIndex")) activeTabIndex

viewTab :: Config i -> Tab.Config i -> HH.HTML w i
viewTab barConfig tabConfig =
  HH.button
    ( Array.catMaybes
        [ Just $ HP.class_ $ Array.concat
          [ [ mdc_tab ]
          , if barConfig.stacked then [ mdc_tab____stacked ] else []
          , if barConfig.minWidth then [ mdc_tab____min_width ] else []
          ]
        , Just (HP.attr (AttrName "role") "tab")
        , map (HE.handler "MDCTab:interacted" <<< Decode.succeed) tabConfig.onClick
        ]
        <> tabConfig.additionalAttributes
    )
    ( Array.catMaybes
        $ if barConfig.indicatorSpansContent then
            [ tabContentElt barConfig tabConfig tabConfig.content
            , tabRippleElt
            ]
          else
            [ tabContentElt barConfig tabConfig tabConfig.content
            , tabIndicatorElt tabConfig
            , tabRippleElt
            ]
    )

tabContentElt :: Config i -> Tab.Config.Config i -> Tab.Config.Content -> HH.HTML w i
tabContentElt barConfig config content =
    HH.div [ HP.class_ mdc_tab__content ]
        ( if barConfig.indicatorSpansContent then
            Array.catMaybes
              [ tabIconElt content
              , tabTextLabelElt content
              , tabIndicatorElt config
              ]
          else
            Array.catMaybes
              [ tabIconElt content
              , tabTextLabelElt content
              ]
        )

tabIconElt :: Tab.Config.Content -> Maybe (HH.HTML w i)
tabIconElt configContent =
  map
    ( \iconName ->
        HH.span
          [ HP.class_ mdc_tab__icon material_icons ]
          [ HH.text iconName ]
    )
    configContent.icon

tabTextLabelElt :: Tab.Config.Content -> HH.HTML w i
tabTextLabelElt configContent = HH.span [ HP.class_ mdc_tab__text_label ] [ HH.text configContent.label ]

tabIndicatorElt :: HH.HTML w i
tabIndicatorElt =
  HH.span
  [ HP.class_ mdc_tab_indicator ]
  [ HH.span
    [ HP.classes [ mdc_tab_indicator__content, mdc_tab_indicator__content____underline ] ]
    []
  ]

tabRippleElt :: HH.HTML w i
tabRippleElt = HH.span [ HP.class_ mdc_tab__ripple ] []

data Align
  = Start
  | End
  | Center

tabScroller :: Config i -> Maybe Align -> Array (Tab.Config i) -> HH.HTML w i
tabScroller config align tabs =
  HH.div
    [ HP.classes $
      [ mdc_tab_scroller ]
      <>
      ( case align of
              Just Start -> [ mdc_tab_scroller____align_start ]
              Just End -> [ mdc_tab_scroller____align_end ]
              Just Center -> [ mdc_tab_scroller____align_center ]
              Nothing -> []
      )
    ]
    [ tabScrollerScrollAreaElt config tabs ]

tabScrollerScrollAreaElt :: Config i -> Array (Tab.Config i) -> HH.HTML w i
tabScrollerScrollAreaElt barConfig tabs = HH.div [ HP.class_ mdc_tab_scroller__scroll_area ] [ tabScrollerScrollContentElt barConfig tabs ]

tabScrollerScrollContentElt :: Config i -> Array (Tab.Config i) -> HH.HTML w i
tabScrollerScrollContentElt barConfig tabs = HH.div [ HP.class_ mdc_tab_scroller__scroll_content ] (map (viewTab barConfig) tabs)
