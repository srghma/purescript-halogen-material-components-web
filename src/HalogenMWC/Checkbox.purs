module HalogenMWC.Checkbox where

import Prelude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen (AttrName(..), ClassName(..), ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.SVG.Attributes as Halogen.SVG.Attributes
import Halogen.SVG.Elements as Halogen.SVG.Elements
import HalogenMWC.Utils (checkboxChangeHandler) as Utils
import Material.Classes.Checkbox (mdc_checkbox, mdc_checkbox____touch, mdc_checkbox__background, mdc_checkbox__checkmark, mdc_checkbox__checkmark_path, mdc_checkbox__mixedmark, mdc_checkbox__native_control, mdc_touch_target_wrapper)
import Web.Event.Event (Event)

type Config i =
  { state :: Maybe State
  , disabled :: Boolean
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLinput i)
  , onChange :: Maybe (Event -> i)
  , touch :: Boolean
  }

data State
  = Unchecked
  | Checked
  | Indeterminate

derive instance eqState :: Eq State

defaultConfig :: forall i. Config i
defaultConfig =
  { state: Nothing
  , disabled: false
  , additionalClasses: []
  , additionalAttributes: []
  , onChange: Nothing
  , touch: true
  }

checkbox :: forall w i. Config i -> HH.HTML w i
checkbox config =
  let
    wrapTouch node =
      if config.touch then
        HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]
      else
        node
  in
    wrapTouch
      $ HH.element (ElemName "mdc-checkbox")
          ( [ HP.classes $ Array.concat
                [ [ mdc_checkbox ]
                  , (if config.touch then [ mdc_checkbox____touch ] else [])
                  , config.additionalClasses
                ]
            , HP.checked (config.state == Just Checked)
            , HP.prop (PropName "indeterminate") (config.state == Just Indeterminate)
            , HP.disabled config.disabled
            ]
            <> config.additionalAttributes
          )
          [ nativeControlElt config
          , backgroundElt
          ]

-- Note: MDCArray choses to send a change event to all checkboxes, thus we
-- have to check here if the state actually changed.
changeHandler :: forall i r. Maybe (Event -> i) -> Array (IProp r i)
changeHandler = case _ of
  Nothing -> []
  Just onChange -> [ Utils.checkboxChangeHandler onChange ]

nativeControlElt :: forall w i. Config i -> HH.HTML w i
nativeControlElt config =
  HH.input
    ( [ HP.type_ InputCheckbox
      , HP.classes [ mdc_checkbox__native_control ]
      , HP.checked (config.state == Just Checked)
      , HP.prop (PropName "indeterminate") (config.state == Just Indeterminate)
      ]
        <> changeHandler config.onChange
    )

backgroundElt :: forall w i. HH.HTML w i
backgroundElt =
  HH.div
    [ HP.class_ mdc_checkbox__background ]
    [ Halogen.SVG.Elements.svg
        [ Halogen.SVG.Attributes.class_ mdc_checkbox__checkmark
        , HP.attr (AttrName "viewBox") "0 0 24 24"
        ]
        [ Halogen.SVG.Elements.path
            [ Halogen.SVG.Attributes.class_ mdc_checkbox__checkmark_path
            , HP.attr (AttrName "fill") "none"
            , HP.attr (AttrName "d") "M1.73,12.91 8.1,19.28 22.79,4.59"
            ]
        ]
    , HH.div [ HP.class_ mdc_checkbox__mixedmark ] []
    ]
