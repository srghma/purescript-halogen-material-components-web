module HalogenMWC.Button
    ( Config, config







    , text, outlined, raised, unelevated
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



import HalogenMWC.Button.Internal (Config(..))



type Config r i =
    Material.Button.Internal.Config r i



config :: Config r i
config =
    Config
        { icon = Nothing
        , trailingIcon = False
        , disabled = False
        , dense = False
        , href = Nothing
        , target = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        , touch = True
        }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setTrailingIcon :: Boolean -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }



setDense :: Boolean -> Config r i -> Config r i
setDense dense (Config config_) =
    Config { config_ | dense = dense }



setHref :: Maybe String -> Config r i -> Config r i
setHref href (Config config_) =
    Config { config_ | href = href }



setTarget :: Maybe String -> Config r i -> Config r i
setTarget target (Config config_) =
    Config { config_ | target = target }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Specify whether touch support is enabled (enabled by default)

Touch support is an accessibility guideline that states that touch targets
should be at least 48 x 48 pixels in size. Use this configuration option to
disable increased touch target size.

**Note:** Buttons with touch support will be wrapped in a HTML div element to
prevent potentially overlapping touch targets on adjacent elements.

-}
setTouch :: Boolean -> Config r i -> Config r i
setTouch touch (Config config_) =
    Config { config_ | touch = touch }


data Variant
    = Text
    | Raised
    | Unelevated
    | Outlined


button :: Variant -> Config r i -> String -> Html r i
button variant ((Config { additionalAttributes, touch, href }) as config_) label =
    let
        wrapTouch node =
            if touch then
                Html.div [ class "mdc-touch-target-wrapper" ] [ node ]

            else
                node
    in
    wrapTouch <|
        Html.node "mdc-button"
            (Array.filterMap identity [ disabledProp config_ ])
            [ (if href /= Nothing then
                Html.a

               else
                Html.button
              )
                (Array.filterMap identity
                    [ rootCs
                    , variantCs variant
                    , denseCs config_
                    , touchCs config_
                    , disabledAttr config_
                    , tabIndexProp config_
                    , hrefAttr config_
                    , targetAttr config_
                    , clickHandler config_
                    ]
                    ++ additionalAttributes
                )
                (Array.filterMap identity
                    [ rippleElt
                    , leadingIconElt config_
                    , labelElt label
                    , trailingIconElt config_
                    , touchElt config_
                    ]
                )
            ]


{-| Text button variant (flush without outline)
-}
text :: Config r i -> String -> Html r i
text config_ label =
    button Text config_ label


{-| Outlined button variant (flush with outline)
-}
outlined :: Config r i -> String -> Html r i
outlined config_ label =
    button Outlined config_ label


{-| Raised button variant (contained with elevation)
-}
raised :: Config r i -> String -> Html r i
raised config_ label =
    button Raised config_ label


{-| Unelevated button variant (contained without elevation)
-}
unelevated :: Config r i -> String -> Html r i
unelevated config_ label =
    button Unelevated config_ label


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-button")


disabledProp :: Config r i -> Maybe (Html.Attribute r i)
disabledProp (Config { disabled }) =
    Just (Html.Attributes.property "disabled" (Encode.bool disabled))


disabledAttr :: Config r i -> Maybe (Html.Attribute r i)
disabledAttr (Config { disabled }) =
    Just (Html.Attributes.disabled disabled)


tabIndexProp :: Config r i -> Maybe (Html.Attribute r i)
tabIndexProp (Config { disabled }) =
    if disabled then
        Just (Html.Attributes.property "tabIndex" (Encode.int -1))

    else
        Just (Html.Attributes.property "tabIndex" (Encode.int 0))


hrefAttr :: Config r i -> Maybe (Html.Attribute r i)
hrefAttr (Config { href }) =
    Maybe.map Html.Attributes.href href


targetAttr :: Config r i -> Maybe (Html.Attribute r i)
targetAttr (Config { href, target }) =
    if href /= Nothing then
        Maybe.map Html.Attributes.target target

    else
        Nothing


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick


variantCs :: Variant -> Maybe (Html.Attribute r i)
variantCs variant =
    case variant of
        Text ->
            Nothing

        Raised ->
            Just (class "mdc-button--raised")

        Unelevated ->
            Just (class "mdc-button--unelevated")

        Outlined ->
            Just (class "mdc-button--outlined")


denseCs :: Config r i -> Maybe (Html.Attribute r i)
denseCs (Config { dense }) =
    if dense then
        Just (class "mdc-button--dense")

    else
        Nothing


touchCs :: Config r i -> Maybe (Html.Attribute r i)
touchCs (Config { touch }) =
    if touch then
        Just (class "mdc-button--touch")

    else
        Nothing


iconElt :: Config r i -> Maybe (Html r i)
iconElt (Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i
                [ class "mdc-button__icon material-icons"
                , Html.Attributes.attribute "aria-hidden" "true"
                ]
                [ Html.text iconName ]
        )
        icon


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ class "mdc-button__ripple" ] [])


leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt ((Config { trailingIcon }) as config_) =
    if not trailingIcon then
        iconElt config_

    else
        Nothing


trailingIconElt :: Config r i -> Maybe (Html r i)
trailingIconElt ((Config { trailingIcon }) as config_) =
    if trailingIcon then
        iconElt config_

    else
        Nothing


touchElt :: Config r i -> Maybe (Html r i)
touchElt (Config { touch }) =
    if touch then
        Just (Html.div [ class "mdc-button__touch" ] [])

    else
        Nothing


labelElt :: String -> Maybe (Html r i)
labelElt label =
    Just (Html.span [ class "mdc-button__label" ] [ Html.text label ])
