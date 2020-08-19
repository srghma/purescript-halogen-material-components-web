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



setColor :: Maybe Color -> Config r i -> Config r i
setColor color (Config config_) =
    Config { config_ | color = color }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



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
ripple isUnbounded ((Config { additionalAttributes }) as config_) =
    Html.node "mdc-ripple"
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
            ++ additionalAttributes
        )
        []



bounded :: Config r i -> Html r i
bounded =
    ripple False



unbounded :: Config r i -> Html r i
unbounded =
    ripple True


rippleSurface :: Maybe (Html.Attribute r i)
rippleSurface =
    Just (class "mdc-ripple-surface")


colorCs :: Config r i -> Maybe (Html.Attribute r i)
colorCs (Config { color }) =
    case color of
        Just Primary ->
            Just (class "mdc-ripple-surface--primary")

        Just Accent ->
            Just (class "mdc-ripple-surface--accent")

        Nothing ->
            Nothing


unboundedProp :: Boolean -> Maybe (Html.Attribute r i)
unboundedProp isUnbounded =
    Just (Html.Attributes.property "unbounded" (Encode.bool isUnbounded))


unboundedData :: Boolean -> Maybe (Html.Attribute r i)
unboundedData isUnbounded =
    if isUnbounded then
        Just (Html.Attributes.attribute "data-mdc-ripple-is-unbounded" "")

    else
        Nothing
