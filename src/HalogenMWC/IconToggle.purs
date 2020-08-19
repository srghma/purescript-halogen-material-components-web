module HalogenMWC.IconToggle
    ( Config, config





    , iconToggle
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { on :: Boolean
        , disabled :: Boolean
        , label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        }



config :: Config r i
config =
    Config
        { on = False
        , disabled = False
        , label = Nothing
        , additionalAttributes = []
        , onChange = Nothing
        }












{-| Specify the HTML5 aria-label attribute of an icon toggle
-}















iconToggle :: Config r i -> { onIcon :: String, offIcon :: String } -> Html r i
iconToggle ({ additionalAttributes } as config_) { onIcon, offIcon } =
    HH.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , onProp config_
            , tabIndexProp
            , ariaHiddenAttr
            , ariaPressedAttr config_
            , ariaLabelAttr config_
            , changeHandler config_
            , disabledAttr config_
            ]
            <> additionalAttributes
        )
        [ HH.i (Array.filterMap identity [ materialIconsCs, onIconCs ]) [ text onIcon ]
        , HH.i (Array.filterMap identity [ materialIconsCs, iconCs ]) [ text offIcon ]
        ]


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_icon_button)


onProp :: Config r i -> Maybe (HH.Attribute r i)
onProp { on } =
    Just (HH.Attributes.property "on" (Encode.bool on))


materialIconsCs :: Maybe (HH.Attribute r i)
materialIconsCs =
    Just (HP.class_ material_icons)


iconCs :: Maybe (HH.Attribute r i)
iconCs =
    Just (HP.class_ mdc_icon_button__icon)


onIconCs :: Maybe (HH.Attribute r i)
onIconCs =
    Just (HP.class_ "mdc-icon-button__icon mdc-icon-button__icon--on")


tabIndexProp :: Maybe (HH.Attribute r i)
tabIndexProp =
    Just (HH.Attributes.tabindex 0)


ariaHiddenAttr :: Maybe (HH.Attribute r i)
ariaHiddenAttr =
    Just (HH.Attributes.attribute "aria-hidden" "true")


ariaPressedAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaPressedAttr { on } =
    Just
        (HH.Attributes.attribute "aria-pressed"
            (if on then
                "true"

             else
                "false"
            )
        )


ariaLabelAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaLabelAttr { label } =
    Maybe.map (HH.Attributes.attribute "aria-label") label


changeHandler :: Config r i -> Maybe (HH.Attribute r i)
changeHandler { onChange } =
    Maybe.map (HH.Events.on "MDCIconButtonToggle:change" << Decode.succeed)
        onChange


disabledAttr :: Config r i -> Maybe (HH.Attribute r i)
disabledAttr { disabled } =
    Just (HH.Attributes.disabled disabled)
