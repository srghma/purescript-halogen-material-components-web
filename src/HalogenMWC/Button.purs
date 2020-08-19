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



data Variant
    = Text
    | Raised
    | Unelevated
    | Outlined


button :: Variant -> Config r i -> String -> Html r i
button variant (config_@{ additionalAttributes, touch, href }) label =
    let
        wrapTouch node =
            if touch then
                HH.div [ HP.class_ mdc_touch_target_wrapper ] [ node ]

            else
                node
    in
    wrapTouch $
        HH.node "mdc-button"
            (Array.filterMap identity [ disabledProp config_ ])
            [ (if href /= Nothing then
                HH.a
               else
                HH.button
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
                    <> additionalAttributes
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


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_button)


disabledProp :: Config r i -> Maybe (HH.Attribute r i)
disabledProp { disabled } =
    Just (HH.Attributes.property "disabled" (Encode.bool disabled))


disabledAttr :: Config r i -> Maybe (HH.Attribute r i)
disabledAttr { disabled } =
    Just (HH.Attributes.disabled disabled)


tabIndexProp :: Config r i -> Maybe (HH.Attribute r i)
tabIndexProp { disabled } =
    if disabled then
        Just (HH.Attributes.property "tabIndex" (Encode.int -1))

    else
        Just (HH.Attributes.property "tabIndex" (Encode.int 0))


hrefAttr :: Config r i -> Maybe (HH.Attribute r i)
hrefAttr { href } =
    Maybe.map HH.Attributes.href href


targetAttr :: Config r i -> Maybe (HH.Attribute r i)
targetAttr { href, target } =
    if href /= Nothing then
        Maybe.map HH.Attributes.target target

    else
        Nothing


clickHandler :: Config r i -> Maybe (HH.Attribute r i)
clickHandler { onClick } =
    Maybe.map HH.Events.onClick onClick


variantCs :: Variant -> Maybe (HH.Attribute r i)
variantCs variant =
    case variant of
        Text ->
            Nothing

        Raised ->
            Just (HP.class_ mdc_button____raised)

        Unelevated ->
            Just (HP.class_ mdc_button____unelevated)

        Outlined ->
            Just (HP.class_ mdc_button____outlined)


denseCs :: Config r i -> Maybe (HH.Attribute r i)
denseCs { dense } =
    if dense then
        Just (HP.class_ mdc_button____dense)

    else
        Nothing


touchCs :: Config r i -> Maybe (HH.Attribute r i)
touchCs { touch } =
    if touch then
        Just (HP.class_ mdc_button____touch)

    else
        Nothing


iconElt :: Config r i -> Maybe (Html r i)
iconElt { icon } =
    Maybe.map
        (\iconName ->
            HH.i
                [ HP.class_ "mdc-button__icon material-icons"
                , HH.Attributes.attribute "aria-hidden" "true"
                ]
                [ HH.text iconName ]
        )
        icon


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_button__ripple ] [])


leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt ({ trailingIcon } as config_) =
    if not trailingIcon then
        iconElt config_

    else
        Nothing


trailingIconElt :: Config r i -> Maybe (Html r i)
trailingIconElt ({ trailingIcon } as config_) =
    if trailingIcon then
        iconElt config_

    else
        Nothing


touchElt :: Config r i -> Maybe (Html r i)
touchElt { touch } =
    if touch then
        Just (HH.div [ HP.class_ mdc_button__touch ] [])

    else
        Nothing


labelElt :: String -> Maybe (Html r i)
labelElt label =
    Just (HH.span [ HP.class_ mdc_button__label ] [ HH.text label ])
