module HalogenMWC.IconToggle where

import Material.Classes.IconButton
import MaterialIconsFont.Classes
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
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
    ( Array.catMaybes
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
    [ HH.i (Array.catMaybes [ materialIconsCs, onIconCs ]) [ HH.text onIcon ]
    , HH.i (Array.catMaybes [ materialIconsCs, iconCs ]) [ HH.text offIcon ]
    ]

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_icon_button)

onProp :: Config r i -> Maybe (IProp r i)
onProp { on } = Just (HP.prop "on" on)

materialIconsCs :: Maybe (IProp r i)
materialIconsCs = Just (HP.class_ material_icons)

iconCs :: Maybe (IProp r i)
iconCs = Just (HP.class_ mdc_icon_button__icon)

onIconCs :: Maybe (IProp r i)
onIconCs = Just (HP.class_ [ mdc_icon_button__icon, mdc_icon_button__icon____on ])

tabIndexProp :: Maybe (IProp r i)
tabIndexProp = Just (HP.tabindex 0)

ariaHiddenAttr :: Maybe (IProp r i)
ariaHiddenAttr = Just (HP.attr "aria-hidden" "true")

ariaPressedAttr :: Config r i -> Maybe (IProp r i)
ariaPressedAttr { on } =
  Just
    ( HP.attr "aria-pressed"
        ( if on then
            "true"
          else
            "false"
        )
    )

ariaLabelAttr :: Config r i -> Maybe (IProp r i)
ariaLabelAttr { label } = map (HP.attr "aria-label") label

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { onChange } =
  map (HH.Events.on "MDCIconButtonToggle:change" <<< Decode.succeed)
    onChange

disabledAttr :: Config r i -> Maybe (IProp r i)
disabledAttr { disabled } = Just (HP.disabled disabled)
