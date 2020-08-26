module HalogenMWC.TabBar where

import Halogen (AttrName(..), ElemName(..), PropName(..))
import Material.Classes.Tab (mdc_tab, mdc_tab____min_width, mdc_tab____stacked, mdc_tab__content, mdc_tab__icon, mdc_tab__ripple, mdc_tab__text_label)
import Material.Classes.TabIndicator (mdc_tab_indicator, mdc_tab_indicator__content, mdc_tab_indicator__content____underline)
import Material.Classes.TabScroller (mdc_tab_scroller, mdc_tab_scroller____align_center, mdc_tab_scroller____align_end, mdc_tab_scroller____align_start, mdc_tab_scroller__scroll_area, mdc_tab_scroller__scroll_content)
import Protolude (Maybe(..), map, ($), (<>))
import MaterialIconsFont.Classes (material_icons)

import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Tab as Tab
import Material.Classes.TabBar (mdc_tab_bar)
import Web.Event.Event (EventType(..))

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

data Align
  = Start
  | End
  | Center

tabScroller :: forall w i . Config i -> Maybe Align -> Array (Tab.Config i) -> HH.HTML w i
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

tabScrollerScrollAreaElt :: forall w i . Config i -> Array (Tab.Config i) -> HH.HTML w i
tabScrollerScrollAreaElt barConfig tabs = HH.div [ HP.class_ mdc_tab_scroller__scroll_area ] [ tabScrollerScrollContentElt barConfig tabs ]

tabScrollerScrollContentElt :: forall w i . Config i -> Array (Tab.Config i) -> HH.HTML w i
tabScrollerScrollContentElt barConfig tabs = HH.div [ HP.class_ mdc_tab_scroller__scroll_content ] (map (viewTab barConfig) tabs)

viewTab :: forall w i . Config i -> Tab.Config i -> HH.HTML w i
viewTab barConfig tabConfig =
  HH.button
    ( Array.catMaybes
        [ Just $ HP.classes $ Array.catMaybes
          [ Just mdc_tab
          , if barConfig.stacked then Just mdc_tab____stacked else Nothing
          , if barConfig.minWidth then Just mdc_tab____min_width else Nothing
          ]
        , Just (HP.attr (AttrName "role") "tab")
        , map (HE.handler (EventType "MDCTab:interacted")) tabConfig.onClick
        ]
        <> tabConfig.additionalAttributes
    )
    ( if barConfig.indicatorSpansContent then
        [ tabContentElt barConfig tabConfig tabConfig.content
        , tabRippleElt
        ]
      else
        [ tabContentElt barConfig tabConfig tabConfig.content
        , tabIndicatorElt
        , tabRippleElt
        ]
    )

tabRippleElt :: forall w i . HH.HTML w i
tabRippleElt = HH.span [ HP.class_ mdc_tab__ripple ] []

tabContentElt :: forall w i . Config i -> Tab.Config i -> Tab.Content -> HH.HTML w i
tabContentElt barConfig config content =
    HH.div
    [ HP.class_ mdc_tab__content ]
    ( if barConfig.indicatorSpansContent then
        Array.catMaybes
          [ tabIconElt content.icon
          , Just $ tabTextLabelElt content.label
          , Just $ tabIndicatorElt
          ]
      else
        Array.catMaybes
          [ tabIconElt content.icon
          , Just $ tabTextLabelElt content.label
          ]
    )

tabIconElt :: forall w i . Maybe String -> Maybe (HH.HTML w i)
tabIconElt =
  map
    ( \iconName ->
        HH.span
          [ HP.classes [ mdc_tab__icon, material_icons ] ]
          [ HH.text iconName ]
    )

tabTextLabelElt :: forall w i . String -> HH.HTML w i
tabTextLabelElt label = HH.span [ HP.class_ mdc_tab__text_label ] [ HH.text label ]

tabIndicatorElt :: forall w i . HH.HTML w i
tabIndicatorElt =
  HH.span
  [ HP.class_ mdc_tab_indicator ]
  [ HH.span
    [ HP.classes [ mdc_tab_indicator__content, mdc_tab_indicator__content____underline ] ]
    []
  ]
