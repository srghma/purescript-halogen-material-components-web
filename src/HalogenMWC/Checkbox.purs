module HalogenMWC.Checkbox
    ( Config, config

    , State



    , checkbox
    , checked, unchecked
    , indeterminate
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Checkbox.Internal (Config(..), State(..))
import Svg
import Svg.Attributes



type Config r i =
    Material.Checkbox.Internal.Config r i



config :: Config r i
config =
    Config
        { state = Nothing
        , disabled = False
        , additionalAttributes = []
        , onChange = Nothing
        , touch = True
        }



setState :: Maybe State -> Config r i -> Config r i
setState state (Config config_) =
    Config { config_ | state = state }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: r i -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }


{-| Specify whether touch support is enabled (enabled by default)

Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.

**Note:** Checkboxes with touch support will be wrapped in a HTML div element
to prevent potentially overlapping touch targets on adjacent elements.

-}
setTouch :: Boolean -> Config r i -> Config r i
setTouch touch (Config config_) =
    Config { config_ | touch = touch }



data State =
    Material.Checkbox.Internal.State



unchecked :: State
unchecked =
    Unchecked



checked :: State
checked =
    Checked



indeterminate :: State
indeterminate =
    Indeterminate



checkbox :: Config r i -> Html r i
checkbox ((Config { touch, additionalAttributes }) as config_) =
    let
        wrapTouch node =
            if touch then
                Html.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]

            else
                node
    in
    wrapTouch $
        Html.node "mdc-checkbox"
            (Array.filterMap identity
                [ rootCs
                , touchCs config_
                , checkedProp config_
                , indeterminateProp config_
                , disabledProp config_
                ]
                ++ additionalAttributes
            )
            [ nativeControlElt config_
            , backgroundElt
            ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_checkbox)


touchCs :: Config r i -> Maybe (Html.Attribute r i)
touchCs (Config { touch }) =
    if touch then
        Just (HP.class_ mdc_checkbox____touch)

    else
        Nothing


checkedProp :: Config r i -> Maybe (Html.Attribute r i)
checkedProp (Config { state }) =
    Just (Html.Attributes.property "checked" (Encode.bool (state == Just Checked)))


indeterminateProp :: Config r i -> Maybe (Html.Attribute r i)
indeterminateProp (Config { state }) =
    Just (Html.Attributes.property "indeterminate" (Encode.bool (state == Just Indeterminate)))


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { state, onChange }) =
    -- Note: MDCArray choses to send a change event to all checkboxes, thus we
    -- have to check here if the state actually changed.
    Maybe.map
        (\r i ->
            Html.Events.on "change"
                (Decode.at [ "target", "checked" ] Decode.bool
                    # Decode.andThen
                        (\isChecked ->
                            if
                                (isChecked && state /= Just Checked)
                                    || (not isChecked && state /= Just Unchecked)
                            then
                                Decode.succeed r i

                            else
                                Decode.fail ""
                        )
                )
        )
        onChange


nativeControlElt :: Config r i -> Html r i
nativeControlElt config_ =
    Html.input
        (Array.filterMap identity
            [ Just (Html.Attributes.type_ "checkbox")
            , Just (HP.class_ mdc_checkbox__native_control)
            , checkedProp config_
            , indeterminateProp config_
            , changeHandler config_
            ]
        )
        []


backgroundElt :: Html r i
backgroundElt =
    Html.div
        [ HP.class_ mdc_checkbox__background ]
        [ Svg.svg
            [ Svg.Attributes.class_ "mdc-checkbox__checkmark"
            , Svg.Attributes.viewBox "0 0 24 24"
            ]
            [ Svg.path
                [ Svg.Attributes.class_ "mdc-checkbox__checkmark-path"
                , Svg.Attributes.fill "none"
                , Svg.Attributes.d "M1.73,12.91 8.1,19.28 22.79,4.59"
                ]
                []
            ]
        , Html.div [ HP.class_ mdc_checkbox__mixedmark ] []
        ]
