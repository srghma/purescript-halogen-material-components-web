module HalogenMWC.Radio
  ( Config
  , config
  , radio
  ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
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
  { checked: False
  , disabled: False
  , additionalAttributes: []
  , onChange: Nothing
  , touch: True
  }

{-| Specify whether touch support is enabled (enabled by default)

Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.

**Note:** Radios with touch support will be wrapped in a HTML div element to
prevent potentially overlapping touch targets on adjacent elements.

-}
radio :: Config r i -> Html r i
radio (config_@{ touch, additionalAttributes }) =
  let
    wrapTouch node =
      if touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.node "mdc-radio"
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

rootCs :: Maybe (HH.Attribute r i)
rootCs = Just (HP.class_ mdc_radio)

touchCs :: Config r i -> Maybe (HH.Attribute r i)
touchCs { touch } =
  if touch then
    Just (HP.class_ mdc_radio____touch)
  else
    Nothing

checkedProp :: Config r i -> Maybe (HH.Attribute r i)
checkedProp { checked } = Just (HH.Attributes.property "checked" (Encode.bool checked))

disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp { disabled } = Just (HH.Attributes.property "disabled" (Encode.bool disabled))

changeHandler :: Config r i -> Maybe (HH.Attribute r i)
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

nativeControlElt :: Config r i -> Html r i
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

nativeControlCs :: Maybe (HH.Attribute r i)
nativeControlCs = Just (HP.class_ mdc_radio__native_control)

radioTypeAttr :: Maybe (HH.Attribute r i)
radioTypeAttr = Just (HH.Attributes.type_ "radio")

backgroundElt :: Html r i
backgroundElt = HH.div [ HP.class_ mdc_radio__background ] [ outerCircleElt, innerCircleElt ]

outerCircleElt :: Html r i
outerCircleElt = HH.div [ HP.class_ mdc_radio__outer_circle ] []

innerCircleElt :: Html r i
innerCircleElt = HH.div [ HP.class_ mdc_radio__inner_circle ] []

rippleElt :: Html r i
rippleElt = HH.div [ HP.class_ mdc_radio__ripple ] []
