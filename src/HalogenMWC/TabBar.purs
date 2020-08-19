module HalogenMWC.TabBar
    ( Config, config





    , tabBar
    , Align(..)
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Tab.Internal as Tab



type Config r i
    =
        { stacked :: Boolean
        , minWidth :: Boolean
        , indicatorSpansContent :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , align :: Maybe Align
        }



config :: Config r i
config =
    Config
        { stacked = False
        , minWidth = False
        , indicatorSpansContent = False
        , align = Nothing
        , additionalAttributes = []
        }




























tabBar :: Config r i -> Array (Tab r i) -> Html r i
tabBar ((Config { additionalAttributes, align }) as config_) tabs =
    HH.node "mdc-tab-bar"
        (Array.filterMap identity
            [ rootCs
            , tablistRoleAttr
            , activeTabIndexProp tabs
            ]
            ++ additionalAttributes
        )
        [ tabScroller config_ align tabs ]


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_tab_bar)


tablistRoleAttr :: Maybe (HH.Attribute r i)
tablistRoleAttr =
    Just (HH.Attributes.attribute "role" "tablist")


activeTabIndexProp :: Array (Tab r i) -> Maybe (HH.Attribute r i)
activeTabIndexProp tabs =
    let
        activeTabIndex =
            Array.indexedMap Tuple.pair tabs
                # Array.filter (\( _, Tab (Tab.Config { active }) ) -> active)
                # Array.head
                # Maybe.map Tuple.first
    in
    Maybe.map (HH.Attributes.property "activeTabIndex" << Encode.int) activeTabIndex


viewTab :: Config r i -> Tab r i -> Html r i
viewTab ((Config { indicatorSpansContent }) as barConfig) ((Tab ((Tab.Config { additionalAttributes, content }) as tabConfig)) as tab) =
    HH.button
        (Array.filterMap identity
            [ tabCs
            , tabRoleAttr
            , tabStackedCs barConfig
            , tabMinWidthCs barConfig
            , tabClickHandler tabConfig
            ]
            ++ additionalAttributes
        )
        (Array.filterMap identity $
            if indicatorSpansContent then
                [ tabContentElt barConfig tabConfig content
                , tabRippleElt
                ]

            else
                [ tabContentElt barConfig tabConfig content
                , tabIndicatorElt tabConfig
                , tabRippleElt
                ]
        )


tabCs :: Maybe (HH.Attribute r i)
tabCs =
    Just (HP.class_ mdc_tab)


tabStackedCs :: Config r i -> Maybe (HH.Attribute r i)
tabStackedCs (Config { stacked }) =
    if stacked then
        Just (HP.class_ mdc_tab____stacked)

    else
        Nothing


tabMinWidthCs :: Config r i -> Maybe (HH.Attribute r i)
tabMinWidthCs (Config { minWidth }) =
    if minWidth then
        Just (HP.class_ "mdc-tab--min-width")

    else
        Nothing


tabRoleAttr :: Maybe (HH.Attribute r i)
tabRoleAttr =
    Just (HH.Attributes.attribute "role" "tab")


tabClickHandler :: Tab.Config r i -> Maybe (HH.Attribute r i)
tabClickHandler (Tab.Config { onClick }) =
    Maybe.map (HH.Events.on "MDCTab:interacted" << Decode.succeed) onClick


tabContentElt :: Config r i -> Tab.Config r i -> Tab.Content -> Maybe (Html r i)
tabContentElt ((Config { indicatorSpansContent }) as barConfig) config_ content =
    Just
        (HH.div [ HP.class_ mdc_tab__content ]
            (if indicatorSpansContent then
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


tabIconElt :: Tab.Content -> Maybe (Html r i)
tabIconElt { icon } =
    Maybe.map
        (\iconName ->
            HH.span
                [ HP.class_ "mdc-tab__icon material-icons" ]
                [ text iconName ]
        )
        icon


tabTextLabelElt :: Tab.Content -> Maybe (Html r i)
tabTextLabelElt { label } =
    Just (HH.span [ HP.class_ mdc_tab__text_label ] [ text label ])


tabIndicatorElt :: Tab.Config r i -> Maybe (Html r i)
tabIndicatorElt config_ =
    Just (HH.span [ HP.class_ mdc_tab_indicator ] [ tabIndicatorContentElt ])


tabIndicatorContentElt :: Html r i
tabIndicatorContentElt =
    HH.span
        [ HP.class_ mdc_tab_indicator__content
        , HP.class_ mdc_tab_indicator__content____underline
        ]
        []


tabRippleElt :: Maybe (Html r i)
tabRippleElt =
    Just (HH.span [ HP.class_ mdc_tab__ripple ] [])



data Align
    = Start
    | End
    | Center


tabScroller :: Config r i -> Maybe Align -> Array (Tab r i) -> Html r i
tabScroller config_ align tabs =
    HH.div
        (Array.filterMap identity
            [ tabScrollerCs
            , tabScrollerAlignCs align
            ]
        )
        [ tabScrollerScrollAreaElt config_ tabs ]


tabScrollerCs :: Maybe (HH.Attribute r i)
tabScrollerCs =
    Just (HP.class_ mdc_tab_scroller)


tabScrollerAlignCs :: Maybe Align -> Maybe (HH.Attribute r i)
tabScrollerAlignCs align =
    case align of
        Just Start ->
            Just (HP.class_ "mdc-tab-scroller--align-start")

        Just End ->
            Just (HP.class_ "mdc-tab-scroller--align-end")

        Just Center ->
            Just (HP.class_ "mdc-tab-scroller--align-center")

        Nothing ->
            Nothing


tabScrollerScrollAreaElt :: Config r i -> Array (Tab r i) -> Html r i
tabScrollerScrollAreaElt barConfig tabs =
    HH.div [ HP.class_ mdc_tab_scroller__scroll_area ]
        [ tabScrollerScrollContentElt barConfig tabs ]


tabScrollerScrollContentElt :: Config r i -> Array (Tab r i) -> Html r i
tabScrollerScrollContentElt barConfig tabs =
    HH.div [ HP.class_ mdc_tab_scroller__scroll_content ]
        (Array.map (viewTab barConfig) tabs)
