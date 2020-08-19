module HalogenMWC.Radio
    ( Config, config





    , radio
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { checked :: Boolean
        , disabled :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        , touch :: Boolean
        }



config :: Config r i
config =
    Config
        { checked = False
        , disabled = False
        , additionalAttributes = []
        , onChange = Nothing
        , touch = True
        }



setChecked :: Boolean -> Config r i -> Config r i
setChecked checked (Config config_) =
    Config { config_ | checked = checked }



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

**Note:** Radios with touch support will be wrapped in a HTML div element to
prevent potentially overlapping touch targets on adjacent elements.

-}
setTouch :: Boolean -> Config r i -> Config r i
setTouch touch (Config config_) =
    Config { config_ | touch = touch }



radio :: Config r i -> Html r i
radio ((Config { touch, additionalAttributes }) as config_) =
    let
        wrapTouch node =
            if touch then
                Html.div [ class "mdc-touch-target-wrapper" ] [ node ]

            else
                node
    in
    wrapTouch $
        Html.node "mdc-radio"
            (Array.filterMap identity
                [ rootCs
                , touchCs config_
                , checkedProp config_
                , disabledProp config_
                ]
                ++ additionalAttributes
            )
            [ nativeControlElt config_
            , backgroundElt
            , rippleElt
            ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-radio")


touchCs :: Config r i -> Maybe (Html.Attribute r i)
touchCs (Config { touch }) =
    if touch then
        Just (class "mdc-radio--touch")

    else
        Nothing


checkedProp :: Config r i -> Maybe (Html.Attribute r i)
checkedProp (Config { checked }) =
    Just (Html.Attributes.property "checked" (Encode.bool checked))


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { checked, onChange }) =
    -- Note: MDCArray choses to send a change event to all checkboxes, thus we
    -- have to check here if the state actually changed.
    Maybe.map
        (\r i ->
            Html.Events.on "change"
                (Decode.at [ "target", "checked" ] Decode.bool
                    # Decode.andThen
                        (\checked_ ->
                            if (checked_ && not checked) || (not checked_ && checked) then
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
            [ nativeControlCs
            , radioTypeAttr
            , checkedProp config_
            , changeHandler config_
            ]
        )
        []


nativeControlCs :: Maybe (Html.Attribute r i)
nativeControlCs =
    Just (class "mdc-radio__native-control")


radioTypeAttr :: Maybe (Html.Attribute r i)
radioTypeAttr =
    Just (Html.Attributes.type_ "radio")


backgroundElt :: Html r i
backgroundElt =
    Html.div [ class "mdc-radio__background" ] [ outerCircleElt, innerCircleElt ]


outerCircleElt :: Html r i
outerCircleElt =
    Html.div [ class "mdc-radio__outer-circle" ] []


innerCircleElt :: Html r i
innerCircleElt =
    Html.div [ class "mdc-radio__inner-circle" ] []


rippleElt :: Html r i
rippleElt =
    Html.div [ class "mdc-radio__ripple" ] []
