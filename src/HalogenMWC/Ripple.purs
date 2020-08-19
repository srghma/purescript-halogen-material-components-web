module HalogenMWC.Ripple where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { color :: Maybe Color
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { color: Nothing
  , additionalAttributes: []
  }

data Color
  = Primary
  | Accent

primary :: Color
primary = Primary

accent :: Color
accent = Accent

ripple :: Boolean -> Config r i -> HH.HTML w i
ripple isUnbounded (config_@{ additionalAttributes }) =
  HH.element "mdc-ripple"
    ( Array.filterMap identity
        [ unboundedProp isUnbounded
        , unboundedData isUnbounded
        , colorCs config_
        , rippleSurface
        , Just (style "position" "absolute")
        , Just (style "top" "0")
        , Just (style "left" "0")
        , Just (style "right" "0")
        , Just (style "bottom" "0")
        ]
        <> additionalAttributes
    )
    []

bounded :: Config r i -> HH.HTML w i
bounded = ripple false

unbounded :: Config r i -> HH.HTML w i
unbounded = ripple true

rippleSurface :: Maybe (IProp r i)
rippleSurface = Just (HP.class_ mdc_ripple_surface)

colorCs :: Config r i -> Maybe (IProp r i)
colorCs { color } = case color of
  Just Primary -> Just (HP.class_ mdc_ripple_surface____primary)
  Just Accent -> Just (HP.class_ mdc_ripple_surface____accent)
  Nothing -> Nothing

unboundedProp :: Boolean -> Maybe (IProp r i)
unboundedProp isUnbounded = Just (HH.Attributes.property "unbounded" (Encode.bool isUnbounded))

unboundedData :: Boolean -> Maybe (IProp r i)
unboundedData isUnbounded =
  if isUnbounded then
    Just (HH.Attributes.attribute "data-mdc-ripple-is-unbounded" "")
  else
    Nothing
