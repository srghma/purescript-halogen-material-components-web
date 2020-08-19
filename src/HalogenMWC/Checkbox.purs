module HalogenMWC.Checkbox where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.SVG.Elements as Halogen.SVG.Elements
import Halogen.SVG.Attributes as Halogen.SVG.Attributes

type Config r i
  = { state :: Maybe State
    , disabled :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onChange :: Maybe r i
    , touch :: Boolean
    }

data State
  = Unchecked
  | Checked
  | Indeterminate

defaultConfig :: Config r i
defaultConfig =
  { state: Nothing
  , disabled: false
  , additionalAttributes: []
  , onChange: Nothing
  , touch: true
  }

checkbox :: Config r i -> HH.HTML w i
checkbox (config_@{ touch, additionalAttributes }) =
  let
    wrapTouch node =
      if touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.element "mdc-checkbox"
          ( Array.filterMap identity
              [ rootCs
              , touchCs config_
              , checkedProp config_
              , indeterminateProp config_
              , disabledProp config_
              ]
              <> additionalAttributes
          )
          [ nativeControlElt config_
          , backgroundElt
          ]

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_checkbox)

touchCs :: Config r i -> Maybe (IProp r i)
touchCs { touch } =
  if touch then
    Just (HP.class_ mdc_checkbox____touch)
  else
    Nothing

checkedProp :: Config r i -> Maybe (IProp r i)
checkedProp { state } = Just (HP.prop "checked" (Encode.bool (state == Just Checked)))

indeterminateProp :: Config r i -> Maybe (IProp r i)
indeterminateProp { state } = Just (HP.prop "indeterminate" (Encode.bool (state == Just Indeterminate)))

disabledProp :: Config r i -> Maybe (IProp r i)
disabledProp { disabled } = Just (HP.prop "disabled" (Encode.bool disabled))

changeHandler :: Config r i -> Maybe (IProp r i)
changeHandler { state, onChange } =
  -- Note: MDCArray choses to send a change event to all checkboxes, thus we
  -- have to check here if the state actually changed.
  map
    ( \r i ->
        HH.Events.on "change"
          ( Decode.at [ "target", "checked" ] Decode.bool
              # Decode.andThen
                  ( \isChecked ->
                      if (isChecked && state /= Just Checked)
                        || (not isChecked && state /= Just Unchecked) then
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
        [ Just (HH.Attributes.type_ "checkbox")
        , Just (HP.class_ mdc_checkbox__native_control)
        , checkedProp config_
        , indeterminateProp config_
        , changeHandler config_
        ]
    )
    []

backgroundElt :: HH.HTML w i
backgroundElt =
  HH.div
    [ HP.class_ mdc_checkbox__background ]
    [ Halogen.SVG.Elements.svg
        [ Halogen.SVG.Attributes.class_ "mdc-checkbox__checkmark"
        , Halogen.SVG.Attributes.viewBox "0 0 24 24"
        ]
        [ Halogen.SVG.Elements.path
            [ Halogen.SVG.Attributes.class_ "mdc-checkbox__checkmark-path"
            , Halogen.SVG.Attributes.fill "none"
            , Halogen.SVG.Attributes.d "M1.73,12.91 8.1,19.28 22.79,4.59"
            ]
            []
        ]
    , HH.div [ HP.class_ mdc_checkbox__mixedmark ] []
    ]
