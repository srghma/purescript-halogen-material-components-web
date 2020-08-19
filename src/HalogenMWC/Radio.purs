module HalogenMWC.Radio where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML IProp
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { checked :: Boolean
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onChange :: Maybe r i
    , touch :: Boolean
    }

defaultConfig :: Config r i
defaultConfig =
  { checked: false
  , disabled: false
  , additionalAttributes: []
  , onChange: Nothing
  , touch: true
  }

{-| Specify whether touch support is enabled (enabled by default)

Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.

**Note:** Radios with touch support will be wrapped in a HTML div element to
prevent potentially overlapping touch targets on adjacent elements.

-}
radio :: Config r i -> HH.HTML w i
radio (config_@{ touch, additionalAttributes }) =
  let
    wrapTouch node =
      if touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.element "mdc-radio"
          ( Array.filterMap identity
              [ rootCs
              , touchCs config_
              , checkedProp config_
              , disabledProp config_
              ]
              <> additionalAttributes
          )
          [ nativeControlElt config_
          , backgroundElt
          , rippleElt
          ]

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_radio)

touchCs :: Config r i -> Maybe (IProp r i)
touchCs { touch } =
  if touch then
    Just (HP.class_ mdc_radio____touch)
  else
    Nothing

checkedProp :: Config r i -> Maybe (IProp r i)
checkedProp { checked } = Just (HP.prop "checked" checked)

disabledProp :: Config r i -> Maybe (IProp r i)
disabledProp { disabled } = Just (HP.prop "disabled" disabled)

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { checked, onChange } =
  -- Note: MDCArray choses to send a change event to all checkboxes, thus we
  -- have to check here if the state actually changed.
  map
    ( \r i ->
        HH.Events.on "change"
          ( Decode.at [ "target", "checked" ] Decode.bool
              # Decode.andThen
                  ( \checked_ ->
                      if (checked_ && not checked) || (not checked_ && checked) then
                        Decode.succeed r i
                      else
                        Decode.fail ""
                  )
          )
    )
    onChange

nativeControlElt :: Config r i -> HH.HTML w i
nativeControlElt config_ =
  HH.input
    ( Array.filterMap identity
        [ nativeControlCs
        , radioTypeAttr
        , checkedProp config_
        , changeHandler config_
        ]
    )
    []

nativeControlCs :: Maybe (IProp r i)
nativeControlCs = Just (HP.class_ mdc_radio__native_control)

radioTypeAttr :: Maybe (IProp r i)
radioTypeAttr = Just (HH.Attributes.type_ "radio")

backgroundElt :: HH.HTML w i
backgroundElt = HH.div [ HP.class_ mdc_radio__background ] [ outerCircleElt, innerCircleElt ]

outerCircleElt :: HH.HTML w i
outerCircleElt = HH.div [ HP.class_ mdc_radio__outer_circle ] []

innerCircleElt :: HH.HTML w i
innerCircleElt = HH.div [ HP.class_ mdc_radio__inner_circle ] []

rippleElt :: HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_radio__ripple ] []
