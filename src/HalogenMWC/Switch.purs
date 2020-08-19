module HalogenMWC.Switch
    ( switch
    , Config, config




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
        }



config :: Config r i
config =
    Config
        { checked = False
        , disabled = False
        , additionalAttributes = []
        , onChange = Nothing
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



switch :: Config r i -> Html r i
switch ((Config { additionalAttributes }) as config_) =
    HH.node "mdc-switch"
        (Array.filterMap identity
            [ rootCs
            , checkedProp config_
            , disabledProp config_
            ]
            ++ additionalAttributes
        )
        [ trackElt
        , thumbUnderlayElt config_
        ]


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_switch)


checkedProp :: Config r i -> Maybe (HH.Attribute r i)
checkedProp (Config { checked }) =
    Just (HH.Attributes.property "checked" (Encode.bool checked))


disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp (Config { disabled }) =
    Just (HH.Attributes.property "disabled" (Encode.bool disabled))


nativeControlCs :: Maybe (HH.Attribute r i)
nativeControlCs =
    Just (HP.class_ mdc_switch__native_control)


switchRoleAttr :: Maybe (HH.Attribute r i)
switchRoleAttr =
    Just (HH.Attributes.attribute "role" "switch")


checkboxTypeAttr :: Maybe (HH.Attribute r i)
checkboxTypeAttr =
    Just (HH.Attributes.type_ "checkbox")


changeHandler :: Config r i -> Maybe (HH.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (HH.Events.on "change" << Decode.succeed) onChange


trackElt :: Html r i
trackElt =
    HH.div [ HP.class_ mdc_switch__track ] []


thumbUnderlayElt :: Config r i -> Html r i
thumbUnderlayElt config_ =
    HH.div [ HP.class_ mdc_switch__thumb_underlay ] [ thumbElt config_ ]


thumbElt :: Config r i -> Html r i
thumbElt config_ =
    HH.div [ HP.class_ mdc_switch__thumb ] [ nativeControlElt config_ ]


nativeControlElt :: Config r i -> Html r i
nativeControlElt config_ =
    HH.input
        (Array.filterMap identity
            [ nativeControlCs
            , checkboxTypeAttr
            , switchRoleAttr
            , checkedProp config_
            , changeHandler config_
            ]
        )
        []
