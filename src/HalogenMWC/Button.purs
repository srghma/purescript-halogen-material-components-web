module HalogenMWC.Button
  ( module HalogenMWC.Button
  , module Export
  ) where

import Protolude (Maybe(..), negate, ($), (<>))

import DOM.HTML.Indexed as I
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Common as Common
import HalogenMWC.Button.Common (Variant(..))
import HalogenMWC.Button.Common (Variant(..)) as Export

type Config i
  = { icon :: Maybe String
    , trailingIcon :: Boolean
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLbutton i) -- put `HE.onClick (\_ -> Increment)` here
    , touch :: Boolean
    }

defaultConfig :: forall i . Config i
defaultConfig =
  { icon: Nothing
  , trailingIcon: false
  , disabled: false
  , additionalAttributes: []
  , touch: true
  }

button :: forall i w . Variant -> Config i -> String -> HH.HTML w i
button variant config =
  let
    wrapTouch = Common.wrapTouch config.touch
    commonHtml = Common.commonHtml config

    commonProps =
      [ HP.classes (Common.commonClasses variant config.touch)
      , HP.disabled config.disabled
      , HP.prop (PropName "tabIndex") (if config.disabled then -1 else 0)
      ]
  in \label ->
      wrapTouch
        $ HH.element (ElemName "mdc-button")
            [ HP.prop (PropName "disabled") config.disabled
            ]
            [ HH.button (commonProps <> config.additionalAttributes) (commonHtml label)
            ]

-----------------

text :: forall i w . Config i -> String -> HH.HTML w i
text config label = button Text config label

outlined :: forall i w . Config i -> String -> HH.HTML w i
outlined config label = button Outlined config label

raised :: forall i w . Config i -> String -> HH.HTML w i
raised config label = button Raised config label

unelevated :: forall i w . Config i -> String -> HH.HTML w i
unelevated config label = button Unelevated config label
