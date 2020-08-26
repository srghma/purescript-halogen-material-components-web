module HalogenMWC.Radio where

import Material.Classes.Radio (mdc_radio, mdc_radio____touch, mdc_radio__background, mdc_radio__inner_circle, mdc_radio__native_control, mdc_radio__outer_circle, mdc_radio__ripple, mdc_touch_target_wrapper)
import Protolude (Maybe(..), runExcept, (#), ($), (<>), (>>=))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Either (hush) as Either
import Foreign (readBoolean, unsafeToForeign) as Foreign
import Foreign.Index (readProp) as Foreign
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, EventType(..))

type Config i
  = { checked :: Boolean
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLinput i)
    , onChange :: Maybe (Event -> i)
    , touch :: Boolean
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { checked: false
  , disabled: false
  , additionalAttributes: []
  , onChange: Nothing
  , touch: true
  }

radio :: forall w i. Config i -> HH.HTML w i
radio config =
  let
    wrapTouch node =
      if config.touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.element (ElemName "mdc-radio")
          ( [ HP.classes
                $ Array.concat
                    [ [ mdc_radio ]
                    , if config.touch then [ mdc_radio____touch ] else []
                    ]
            , HP.checked config.checked
            , HP.disabled config.disabled
            ]
              <> config.additionalAttributes
          )
          [ nativeControlElt config
          , backgroundElt
          , rippleElt
          ]

-- Note: MDCArray choses to send a change event to all checkboxes, thus we
-- have to check here if the state actually changed.
changeHandler :: forall i r. Maybe (Event -> i) -> Array (IProp r i)
changeHandler = case _ of
  Nothing -> []
  Just onChange ->
    [ HE.handler' (EventType "change") (handle onChange)
    ]
  where
  readChecked :: Event -> Maybe Boolean
  readChecked event = Foreign.unsafeToForeign event # (\f -> Foreign.readProp "target" f >>= Foreign.readProp "checked" >>= Foreign.readBoolean) # runExcept # Either.hush

  handle onChange = \event -> case readChecked event of
    Nothing -> Nothing
    Just _ -> Just $ onChange event

nativeControlElt :: forall w i. Config i -> HH.HTML w i
nativeControlElt config =
  HH.input
    ( [ HP.class_ mdc_radio__native_control
      , HP.type_ InputRadio
      , HP.checked config.checked
      ]
        <> changeHandler config.onChange
    )

backgroundElt :: forall w i. HH.HTML w i
backgroundElt = HH.div [ HP.class_ mdc_radio__background ] [ outerCircleElt, innerCircleElt ]

outerCircleElt :: forall w i. HH.HTML w i
outerCircleElt = HH.div [ HP.class_ mdc_radio__outer_circle ] []

innerCircleElt :: forall w i. HH.HTML w i
innerCircleElt = HH.div [ HP.class_ mdc_radio__inner_circle ] []

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_radio__ripple ] []
