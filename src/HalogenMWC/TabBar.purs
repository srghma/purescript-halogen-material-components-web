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



setStacked :: Boolean -> Config r i -> Config r i
setStacked stacked (Config config_) =
    Config { config_ | stacked = stacked }



setMinWidth :: Boolean -> Config r i -> Config r i
setMinWidth minWidth (Config config_) =
    Config { config_ | minWidth = minWidth }



setIndicatorSpansContent :: Boolean -> Config r i -> Config r i
setIndicatorSpansContent indicatorSpansContent (Config config_) =
    Config { config_ | indicatorSpansContent = indicatorSpansContent }



setAlign :: Maybe Align -> Config r i -> Config r i
setAlign align (Config config_) =
    Config { config_ | align = align }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



tabBar :: Config r i -> Array (Tab r i) -> Html r i
tabBar ((Config { additionalAttributes, align }) as config_) tabs =
    Html.node "mdc-tab-bar"
        (Array.filterMap identity
            [ rootCs
            , tablistRoleAttr
            , activeTabIndexProp tabs
            ]
            ++ additionalAttributes
        )
        [ tabScroller config_ align tabs ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_tab_bar)


tablistRoleAttr :: Maybe (Html.Attribute r i)
tablistRoleAttr =
    Just (Html.Attributes.attribute "role" "tablist")


activeTabIndexProp :: Array (Tab r i) -> Maybe (Html.Attribute r i)
activeTabIndexProp tabs =
    let
        activeTabIndex =
            Array.indexedMap Tuple.pair tabs
                # Array.filter (\( _, Tab (Tab.Config { active }) ) -> active)
                # Array.head
                # Maybe.map Tuple.first
    in
    Maybe.map (Html.Attributes.property "activeTabIndex" << Encode.int) activeTabIndex


viewTab :: Config r i -> Tab r i -> Html r i
viewTab ((Config { indicatorSpansContent }) as barConfig) ((Tab ((Tab.Config { additionalAttributes, content }) as tabConfig)) as tab) =
    Html.button
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


tabCs :: Maybe (Html.Attribute r i)
tabCs =
    Just (HP.class_ mdc_tab)


tabStackedCs :: Config r i -> Maybe (Html.Attribute r i)
tabStackedCs (Config { stacked }) =
    if stacked then
        Just (HP.class_ "mdc-tab--stacked")

    else
        Nothing


tabMinWidthCs :: Config r i -> Maybe (Html.Attribute r i)
tabMinWidthCs (Config { minWidth }) =
    if minWidth then
        Just (HP.class_ "mdc-tab--min-width")

    else
        Nothing


tabRoleAttr :: Maybe (Html.Attribute r i)
tabRoleAttr =
    Just (Html.Attributes.attribute "role" "tab")


tabClickHandler :: Tab.Config r i -> Maybe (Html.Attribute r i)
tabClickHandler (Tab.Config { onClick }) =
    Maybe.map (Html.Events.on "MDCTab:interacted" << Decode.succeed) onClick


tabContentElt :: Config r i -> Tab.Config r i -> Tab.Content -> Maybe (Html r i)
tabContentElt ((Config { indicatorSpansContent }) as barConfig) config_ content =
    Just
        (Html.div [ HP.class_ mdc_tab__content ]
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
            Html.span
                [ HP.class_ "mdc-tab__icon material-icons" ]
                [ text iconName ]
        )
        icon


tabTextLabelElt :: Tab.Content -> Maybe (Html r i)
tabTextLabelElt { label } =
    Just (Html.span [ HP.class_ mdc_tab__text_label ] [ text label ])


tabIndicatorElt :: Tab.Config r i -> Maybe (Html r i)
tabIndicatorElt config_ =
    Just (Html.span [ HP.class_ mdc_tab_indicator ] [ tabIndicatorContentElt ])


tabIndicatorContentElt :: Html r i
tabIndicatorContentElt =
    Html.span
        [ HP.class_ mdc_tab_indicator__content
        , HP.class_ "mdc-tab-indicator__content--underline"
        ]
        []


tabRippleElt :: Maybe (Html r i)
tabRippleElt =
    Just (Html.span [ HP.class_ mdc_tab__ripple ] [])



data Align
    = Start
    | End
    | Center


tabScroller :: Config r i -> Maybe Align -> Array (Tab r i) -> Html r i
tabScroller config_ align tabs =
    Html.div
        (Array.filterMap identity
            [ tabScrollerCs
            , tabScrollerAlignCs align
            ]
        )
        [ tabScrollerScrollAreaElt config_ tabs ]


tabScrollerCs :: Maybe (Html.Attribute r i)
tabScrollerCs =
    Just (HP.class_ mdc_tab_scroller)


tabScrollerAlignCs :: Maybe Align -> Maybe (Html.Attribute r i)
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
    Html.div [ HP.class_ mdc_tab_scroller__scroll_area ]
        [ tabScrollerScrollContentElt barConfig tabs ]


tabScrollerScrollContentElt :: Config r i -> Array (Tab r i) -> Html r i
tabScrollerScrollContentElt barConfig tabs =
    Html.div [ HP.class_ mdc_tab_scroller__scroll_content ]
        (Array.map (viewTab barConfig) tabs)
