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



setOn :: Boolean -> Config r i -> Config r i
setOn on (Config config_) =
    Config { config_ | on = on }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify the HTML5 aria-label attribute of an icon toggle
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: r i -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



iconToggle :: Config r i -> { onIcon :: String, offIcon :: String } -> Html r i
iconToggle ((Config { additionalAttributes }) as config_) { onIcon, offIcon } =
    Html.node "mdc-icon-button"
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
            ++ additionalAttributes
        )
        [ Html.i (Array.filterMap identity [ materialIconsCs, onIconCs ]) [ text onIcon ]
        , Html.i (Array.filterMap identity [ materialIconsCs, iconCs ]) [ text offIcon ]
        ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-icon-button")


onProp :: Config r i -> Maybe (Html.Attribute r i)
onProp (Config { on }) =
    Just (Html.Attributes.property "on" (Encode.bool on))


materialIconsCs :: Maybe (Html.Attribute r i)
materialIconsCs =
    Just (HP.class_ "material-icons")


iconCs :: Maybe (Html.Attribute r i)
iconCs =
    Just (HP.class_ "mdc-icon-button__icon")


onIconCs :: Maybe (Html.Attribute r i)
onIconCs =
    Just (HP.class_ "mdc-icon-button__icon mdc-icon-button__icon--on")


tabIndexProp :: Maybe (Html.Attribute r i)
tabIndexProp =
    Just (Html.Attributes.tabindex 0)


ariaHiddenAttr :: Maybe (Html.Attribute r i)
ariaHiddenAttr =
    Just (Html.Attributes.attribute "aria-hidden" "true")


ariaPressedAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaPressedAttr (Config { on }) =
    Just
        (Html.Attributes.attribute "aria-pressed"
            (if on then
                "true"

             else
                "false"
            )
        )


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { label }) =
    Maybe.map (Html.Attributes.attribute "aria-label") label


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (Html.Events.on "MDCIconButtonToggle:change" << Decode.succeed)
        onChange


disabledAttr :: Config r i -> Maybe (Html.Attribute r i)
disabledAttr (Config { disabled }) =
    Just (Html.Attributes.disabled disabled)
