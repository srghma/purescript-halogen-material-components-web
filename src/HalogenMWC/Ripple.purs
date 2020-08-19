module HalogenMWC.Ripple
    ( Config, config

    , bounded
    , unbounded
    , Color, primary, accent
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
    =
        { color :: Maybe Color
        , additionalAttributes :: Array (IProp r i)
        }

config :: Config r i
config =
    Config
        { color = Nothing
        , additionalAttributes = []
        }

data Color
    = Primary
    | Accent

primary :: Color
primary =
    Primary

accent :: Color
accent =
    Accent

ripple :: Boolean -> Config r i -> Html r i
ripple isUnbounded (config_@{ additionalAttributes }) =
    HH.node "mdc-ripple"
        (Array.filterMap identity
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

bounded :: Config r i -> Html r i
bounded =
    ripple False

unbounded :: Config r i -> Html r i
unbounded =
    ripple True

rippleSurface :: Maybe (HH.Attribute r i)
rippleSurface =
    Just (HP.class_ mdc_ripple_surface)

colorCs :: Config r i -> Maybe (HH.Attribute r i)
colorCs { color } =
    case color of
        Just Primary ->
            Just (HP.class_ mdc_ripple_surface____primary)

        Just Accent ->
            Just (HP.class_ mdc_ripple_surface____accent)

        Nothing ->
            Nothing

unboundedProp :: Boolean -> Maybe (HH.Attribute r i)
unboundedProp isUnbounded =
    Just (HH.Attributes.property "unbounded" (Encode.bool isUnbounded))

unboundedData :: Boolean -> Maybe (HH.Attribute r i)
unboundedData isUnbounded =
    if isUnbounded then
        Just (HH.Attributes.attribute "data-mdc-ripple-is-unbounded" "")

    else
        Nothing
