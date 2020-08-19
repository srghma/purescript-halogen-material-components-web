module HalogenMWC.IconToggle where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { on :: Boolean
    , disabled :: Boolean
    , label :: Maybe String
    , additionalAttributes :: Array (IProp r i)
    , onChange :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { on: false
  , disabled: false
  , label: Nothing
  , additionalAttributes: []
  , onChange: Nothing
  }

{-| Specify the HTML5 aria-label attribute of an icon toggle
-}
iconToggle :: Config r i -> { onIcon :: String, offIcon :: String } -> HH.HTML w i
iconToggle (config_@{ additionalAttributes }) { onIcon, offIcon } =
  HH.element "mdc-icon-button"
    ( Array.filterMap identity
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

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_icon_button)

onProp :: Config r i -> Maybe (IProp r i)
onProp { on } = Just (HP.prop "on" (Encode.bool on))

materialIconsCs :: Maybe (IProp r i)
materialIconsCs = Just (HP.class_ material_icons)

iconCs :: Maybe (IProp r i)
iconCs = Just (HP.class_ mdc_icon_button__icon)

onIconCs :: Maybe (IProp r i)
onIconCs = Just (HP.class_ "mdc-icon-button__icon mdc-icon-button__icon--on")

tabIndexProp :: Maybe (IProp r i)
tabIndexProp = Just (HH.Attributes.tabindex 0)

ariaHiddenAttr :: Maybe (IProp r i)
ariaHiddenAttr = Just (HH.Attributes.attribute "aria-hidden" "true")

ariaPressedAttr :: Config r i -> Maybe (IProp r i)
ariaPressedAttr { on } =
  Just
    ( HH.Attributes.attribute "aria-pressed"
        ( if on then
            "true"
          else
            "false"
        )
    )

ariaLabelAttr :: Config r i -> Maybe (IProp r i)
ariaLabelAttr { label } = map (HH.Attributes.attribute "aria-label") label

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { onChange } =
  map (HH.Events.on "MDCIconButtonToggle:change" << Decode.succeed)
    onChange

disabledAttr :: Config r i -> Maybe (IProp r i)
disabledAttr { disabled } = Just (HH.Attributes.disabled disabled)
