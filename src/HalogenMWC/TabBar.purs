module HalogenMWC.TabBar where

import Protolude

import Halogen (AttrName(..))
import Halogen.HTML IProp
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Tab (Tab(..))
import HalogenMWC.Tab as Tab
import Material.Classes.Tab
import Material.Classes.TabBar

type Config r i
  = { stacked :: Boolean
    , minWidth :: Boolean
    , indicatorSpansContent :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , align :: Maybe Align
    }

defaultConfig :: Config r i
defaultConfig =
  { stacked: false
  , minWidth: false
  , indicatorSpansContent: false
  , align: Nothing
  , additionalAttributes: []
  }

tabBar :: Config r i -> Array (Tab r i) -> HH.HTML w i
tabBar (config_@{ additionalAttributes, align }) tabs =
  HH.element "mdc-tab-bar"
    ( Array.filterMap identity
        [ rootCs
        , tablistRoleAttr
        , activeTabIndexProp tabs
        ]
        <> additionalAttributes
    )
    [ tabScroller config_ align tabs ]

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_tab_bar)

tablistRoleAttr :: Maybe (IProp r i)
tablistRoleAttr = Just (HH.Attributes.attribute "role" "tablist")

activeTabIndexProp :: Array (Tab r i) -> Maybe (IProp r i)
activeTabIndexProp tabs =
  let
    activeTabIndex =
      Array.indexedMap Tuple.pair tabs
        # Array.filter (\(Tuple _ (Tab { active })) -> active)
        # Array.head
        # map Tuple.first
  in
    map (HP.prop "activeTabIndex" << Encode.int) activeTabIndex

viewTab :: Config r i -> Tab r i -> HH.HTML w i
viewTab (barConfig@{ indicatorSpansContent }) (tabConfig@(Tab ({ additionalAttributes, content }))) =
  HH.button
    ( Array.filterMap identity
        [ tabCs
        , tabRoleAttr
        , tabStackedCs barConfig
        , tabMinWidthCs barConfig
        , tabClickHandler tabConfig
        ]
        <> additionalAttributes
    )
    ( Array.filterMap identity
        $ if indicatorSpansContent then
            [ tabContentElt barConfig tabConfig content
            , tabRippleElt
            ]
          else
            [ tabContentElt barConfig tabConfig content
            , tabIndicatorElt tabConfig
            , tabRippleElt
            ]
    )

tabCs :: Maybe (IProp r i)
tabCs = Just (HP.class_ mdc_tab)

tabStackedCs :: Config r i -> Maybe (IProp r i)
tabStackedCs { stacked } =
  if stacked then
    Just (HP.class_ mdc_tab____stacked)
  else
    Nothing

tabMinWidthCs :: Config r i -> Maybe (IProp r i)
tabMinWidthCs { minWidth } =
  if minWidth then
    Just (HP.class_ mdc_tab____min_width)
  else
    Nothing

tabRoleAttr :: Maybe (IProp r i)
tabRoleAttr = Just (HH.Attributes.attribute "role" "tab")

tabClickHandler :: Tab.Config r i -> Maybe (IProp r i)
tabClickHandler { onClick } = map (HH.Events.on "MDCTab:interacted" << Decode.succeed) onClick

tabContentElt :: Config r i -> Tab.Config r i -> Tab.Content -> Maybe (HH.HTML w i)
tabContentElt (barConfig@{ indicatorSpansContent }) config_ content =
  Just
    ( HH.div [ HP.class_ mdc_tab__content ]
        ( if indicatorSpansContent then
            Array.filterMap identity
              [ tabIconElt content
              , tabTextLabelElt content
              , tabIndicatorElt config_
              ]
          else
            Array.filterMap identity
              [ tabIconElt content
              , tabTextLabelElt content
              ]
        )
    )

tabIconElt :: Tab.Content -> Maybe (HH.HTML w i)
tabIconElt { icon } =
  map
    ( \iconName ->
        HH.span
          [ HP.class_ mdc_tab__icon material_icons ]
          [ text iconName ]
    )
    icon

tabTextLabelElt :: Tab.Content -> Maybe (HH.HTML w i)
tabTextLabelElt { label } = Just (HH.span [ HP.class_ mdc_tab__text_label ] [ text label ])

tabIndicatorElt :: Tab.Config r i -> Maybe (HH.HTML w i)
tabIndicatorElt config_ = Just (HH.span [ HP.class_ mdc_tab_indicator ] [ tabIndicatorContentElt ])

tabIndicatorContentElt :: HH.HTML w i
tabIndicatorContentElt =
  HH.span
    [ HP.class_ mdc_tab_indicator__content
    , HP.class_ mdc_tab_indicator__content____underline
    ]
    []

tabRippleElt :: Maybe (HH.HTML w i)
tabRippleElt = Just (HH.span [ HP.class_ mdc_tab__ripple ] [])

data Align
  = Start
  | End
  | Center

tabScroller :: Config r i -> Maybe Align -> Array (Tab r i) -> HH.HTML w i
tabScroller config_ align tabs =
  HH.div
    ( Array.filterMap identity
        [ tabScrollerCs
        , tabScrollerAlignCs align
        ]
    )
    [ tabScrollerScrollAreaElt config_ tabs ]

tabScrollerCs :: Maybe (IProp r i)
tabScrollerCs = Just (HP.class_ mdc_tab_scroller)

tabScrollerAlignCs :: Maybe Align -> Maybe (IProp r i)
tabScrollerAlignCs align = case align of
  Just Start -> Just (HP.class_ mdc_tab_scroller____align_start)
  Just End -> Just (HP.class_ mdc_tab_scroller____align_end)
  Just Center -> Just (HP.class_ mdc_tab_scroller____align_center)
  Nothing -> Nothing

tabScrollerScrollAreaElt :: Config r i -> Array (Tab r i) -> HH.HTML w i
tabScrollerScrollAreaElt barConfig tabs =
  HH.div [ HP.class_ mdc_tab_scroller__scroll_area ]
    [ tabScrollerScrollContentElt barConfig tabs ]

tabScrollerScrollContentElt :: Config r i -> Array (Tab r i) -> HH.HTML w i
tabScrollerScrollContentElt barConfig tabs =
  HH.div [ HP.class_ mdc_tab_scroller__scroll_content ]
    (map (viewTab barConfig) tabs)
