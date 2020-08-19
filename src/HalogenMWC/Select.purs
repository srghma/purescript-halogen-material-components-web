module HalogenMWC.Select
    ( Config, config








    , filled
    , outlined
    , Icon, icon
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


import HalogenMWC.Icon as Icon
import HalogenMWC.Array as Array
import HalogenMWC.Array.Item as ArrayItem
import HalogenMWC.Menu as Menu
import HalogenMWC.Select.Item (SelectItem)
import HalogenMWC.Select.Item.Internal as SelectItem



data Config a r i
    =
        { label :: Maybe String
        , disabled :: Boolean
        , required :: Boolean
        , valid :: Boolean
        , selected :: Maybe a
        , leadingIcon :: Maybe (Icon r i)
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe (a -> r i)
        }



config :: Config a r i
config =
    Config
        { label = Nothing
        , disabled = False
        , required = False
        , valid = True
        , selected = Nothing
        , leadingIcon = Nothing
        , additionalAttributes = []
        , onChange = Nothing
        }



setLabel :: Maybe String -> Config a r i -> Config a r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setSelected :: Maybe a -> Config a r i -> Config a r i
setSelected selected (Config config_) =
    Config { config_ | selected = selected }



setDisabled :: Boolean -> Config a r i -> Config a r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setRequired :: Boolean -> Config a r i -> Config a r i
setRequired required (Config config_) =
    Config { config_ | required = required }



setValid :: Boolean -> Config a r i -> Config a r i
setValid valid (Config config_) =
    Config { config_ | valid = valid }



setLeadingIcon :: Maybe (Icon r i) -> Config a r i -> Config a r i
setLeadingIcon leadingIcon (Config config_) =
    Config { config_ | leadingIcon = leadingIcon }



setAttributes :: Array (IProp r i) -> Config a r i -> Config a r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: (a -> r i) -> Config a r i -> Config a r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }


data Variant
    = Filled
    | Outlined


select :: Variant -> Config a r i -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
select variant ((Config { leadingIcon, selected, additionalAttributes, onChange }) as config_) firstSelectItem remainingSelectItems =
    let
        selectedIndex =
            Array.indexedMap
                (\index (SelectItem.SelectItem (SelectItem.Config { value }) _) ->
                    if Just value == selected then
                        Just index

                    else
                        Nothing
                )
                (firstSelectItem :: remainingSelectItems)
                # Array.filterMap identity
                # Array.head
    in
    Html.node "mdc-select"
        (Array.filterMap identity
            [ rootCs
            , outlinedCs variant
            , leadingIconCs config_
            , disabledProp config_
            , selectedIndexProp selectedIndex
            , validProp config_
            , requiredProp config_
            ]
            ++ additionalAttributes
        )
        [ anchorElt []
            (Array.concat
                [ [ leadingIconElt config_
                  , dropdownIconElt
                  , selectedTextElt
                  ]
                , if variant == Outlined then
                    [ notchedOutlineElt config_ ]

                  else
                    [ floatingLabelElt config_
                    , lineRippleElt
                    ]
                ]
            )
        , menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems
        ]



filled :: Config a r i -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
filled config_ firstSelectItem remainingSelectItems =
    select Filled config_ firstSelectItem remainingSelectItems



outlined :: Config a r i -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
outlined config_ firstSelectItem remainingSelectItems =
    select Outlined config_ firstSelectItem remainingSelectItems



data Icon r i
    = Icon (Html r i)



icon :: Array (IProp r i) -> String -> Icon r i
icon additionalAttributes iconName =
    Icon (Icon.icon (HP.class_ "mdc-select__icon" :: additionalAttributes) iconName)


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-select")


outlinedCs :: Variant -> Maybe (Html.Attribute r i)
outlinedCs variant =
    if variant == Outlined then
        Just (HP.class_ "mdc-select--outlined")

    else
        Nothing


leadingIconCs :: Config a r i -> Maybe (Html.Attribute r i)
leadingIconCs (Config { leadingIcon }) =
    Maybe.map (\_ -> HP.class_ "mdc-select--with-leading-icon") leadingIcon


disabledProp :: Config a r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


validProp :: Config a r i -> Maybe (Html.Attribute r i)
validProp (Config { valid }) =
    Just (Html.Attributes.property "valid" (Encode.bool valid))


selectedIndexProp :: Maybe Int -> Maybe (Html.Attribute r i)
selectedIndexProp selectedIndex =
    Just
        (Html.Attributes.property "selectedIndex"
            (Encode.int (Maybe.withDefault -1 selectedIndex))
        )


requiredProp :: Config a r i -> Maybe (Html.Attribute r i)
requiredProp (Config { required }) =
    Just (Html.Attributes.property "required" (Encode.bool required))


anchorElt :: Array (IProp r i) -> Array (Html r i) -> Html r i
anchorElt additionalAttributes nodes =
    Html.div (HP.class_ "mdc-select__anchor" :: additionalAttributes) nodes


leadingIconElt :: Config a r i -> Html r i
leadingIconElt (Config { leadingIcon }) =
    case leadingIcon of
        Just (Icon icon_) ->
            icon_

        Nothing ->
            text ""


dropdownIconElt :: Html r i
dropdownIconElt =
    Html.i [ HP.class_ "mdc-select__dropdown-icon" ] []


floatingLabelElt :: Config a r i -> Html r i
floatingLabelElt (Config { label }) =
    Html.div [ HP.class_ "mdc-floating-label" ] [ text (Maybe.withDefault "" label) ]


lineRippleElt :: Html r i
lineRippleElt =
    Html.label [ HP.class_ "mdc-line-ripple" ] []


notchedOutlineElt :: Config a r i -> Html r i
notchedOutlineElt (Config { label }) =
    Html.div [ HP.class_ "mdc-notched-outline" ]
        [ Html.div [ HP.class_ "mdc-notched-outline__leading" ] []
        , Html.div [ HP.class_ "mdc-notched-outline__notch" ]
            [ Html.label [ HP.class_ "mdc-floating-label" ]
                [ text (Maybe.withDefault "" label) ]
            ]
        , Html.div [ HP.class_ "mdc-notched-outline__trailing" ] []
        ]


menuElt :: Maybe (Icon r i) -> Maybe a -> Maybe (a -> r i) -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems =
    Menu.menu
        (Menu.config
            # Menu.setAttributes
                [ HP.class_ "mdc-select__menu"
                , style "width" "100%"
                ]
        )
        [ Array.list (Array.config # Array.setWrapFocus True)
            (listItem leadingIcon selected onChange firstSelectItem)
            (Array.map (listItem leadingIcon selected onChange) remainingSelectItems)
        ]


listItem :: Maybe (Icon r i) -> Maybe a -> Maybe (a -> r i) -> SelectItem a r i -> ArrayItem r i
listItem leadingIcon selected onChange (SelectItem.SelectItem config_ nodes) =
    ArrayItem.listItem (listItemConfig selected onChange config_)
        (if leadingIcon /= Nothing then
            ArrayItem.graphic [] [] :: nodes

         else
            nodes
        )


listItemConfig :: Maybe a -> Maybe (a -> r i) -> SelectItem.Config a r i -> ArrayItem.Config r i
listItemConfig selectedValue onChange (SelectItem.Config { value, disabled, additionalAttributes }) =
    ArrayItem.config
        # ArrayItem.setDisabled disabled
        # ArrayItem.setAttributes additionalAttributes
        # (case onChange of
                Just onChange_ ->
                    ArrayItem.setOnClick (onChange_ value)

                Nothing ->
                    identity
           )


selectedTextElt :: Html r i
selectedTextElt =
    Html.div [ HP.class_ "mdc-select__selected-text" ] []
