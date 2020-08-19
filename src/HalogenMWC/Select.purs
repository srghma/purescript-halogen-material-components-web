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
defaultConfig =
    Config
        { label: Nothing
        , disabled: False
        , required: False
        , valid: True
        , selected: Nothing
        , leadingIcon: Nothing
        , additionalAttributes: []
        , onChange: Nothing
        }

data Variant
    = Filled
    | Outlined

select :: Variant -> Config a r i -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
select variant (config_@{ leadingIcon, selected, additionalAttributes, onChange }) firstSelectItem remainingSelectItems =
    let
        selectedIndex =
            Array.indexedMap
                (\index (SelectItem.SelectItem { value } _) ->
                    if Just value == selected then
                        Just index

                    else
                        Nothing
                )
                ([ firstSelectItem, remainingSelectItems)
                # Array.filterMap identity
                # Array.head
    in
    HH.node "mdc-select"
        (Array.filterMap identity
            [ rootCs
            , outlinedCs variant
            , leadingIconCs config_
            , disabledProp config_
            , selectedIndexProp selectedIndex
            , validProp config_
            , requiredProp config_
            ]
            <> additionalAttributes
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
    Icon (Icon.icon (HP.class_ mdc_select__icon] <> additionalAttributes) iconName)

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_select)

outlinedCs :: Variant -> Maybe (HH.Attribute r i)
outlinedCs variant =
    if variant == Outlined then
        Just (HP.class_ mdc_select____outlined)

    else
        Nothing

leadingIconCs :: Config a r i -> Maybe (HH.Attribute r i)
leadingIconCs { leadingIcon } =
    map (\_ -> HP.class_ "mdc-select--with-leading-icon") leadingIcon

disabledProp :: Config a r i -> Maybe (HH.Attribute r i)
disabledProp { disabled } =
    Just (HH.Attributes.property "disabled" (Encode.bool disabled))

validProp :: Config a r i -> Maybe (HH.Attribute r i)
validProp { valid } =
    Just (HH.Attributes.property "valid" (Encode.bool valid))

selectedIndexProp :: Maybe Int -> Maybe (HH.Attribute r i)
selectedIndexProp selectedIndex =
    Just
        (HH.Attributes.property "selectedIndex"
            (Encode.int (Maybe.withDefault -1 selectedIndex))
        )

requiredProp :: Config a r i -> Maybe (HH.Attribute r i)
requiredProp { required } =
    Just (HH.Attributes.property "required" (Encode.bool required))

anchorElt :: Array (IProp r i) -> Array (Html r i) -> Html r i
anchorElt additionalAttributes nodes =
    HH.div ([HP.class_ mdc_select__anchor] <> additionalAttributes) nodes

leadingIconElt :: Config a r i -> Html r i
leadingIconElt { leadingIcon } =
    case leadingIcon of
        Just (Icon icon_) ->
            icon_

        Nothing ->
            text ""

dropdownIconElt :: Html r i
dropdownIconElt =
    HH.i [ HP.class_ mdc_select__dropdown_icon ] []

floatingLabelElt :: Config a r i -> Html r i
floatingLabelElt { label } =
    HH.div [ HP.class_ mdc_floating_label ] [ text (Maybe.withDefault "" label) ]

lineRippleElt :: Html r i
lineRippleElt =
    HH.label [ HP.class_ mdc_line_ripple ] []

notchedOutlineElt :: Config a r i -> Html r i
notchedOutlineElt { label } =
    HH.div [ HP.class_ mdc_notched_outline ]
        [ HH.div [ HP.class_ mdc_notched_outline__leading ] []
        , HH.div [ HP.class_ mdc_notched_outline__notch ]
            [ HH.label [ HP.class_ mdc_floating_label ]
                [ text (Maybe.withDefault "" label) ]
            ]
        , HH.div [ HP.class_ mdc_notched_outline__trailing ] []
        ]

menuElt :: Maybe (Icon r i) -> Maybe a -> Maybe (a -> r i) -> SelectItem a r i -> Array (SelectItem a r i) -> Html r i
menuElt leadingIcon selected onChange firstSelectItem remainingSelectItems =
    Menu.menu
        (Menu.config
            # Menu.setAttributes
                [ HP.class_ mdc_select__menu
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
listItemConfig selectedValue onChange { value, disabled, additionalAttributes } =
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
    HH.div [ HP.class_ mdc_select__selected_text ] []
