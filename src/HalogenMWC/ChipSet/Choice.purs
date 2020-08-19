module HalogenMWC.ChipSet.Choice
    ( Config, config



    , chipSet
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Chip.Choice.Internal as Chip



data Config a r i
    =
        { selected :: Maybe a
        , onChange :: Maybe (a -> r i)
        , toLabel :: a -> String
        , additionalAttributes :: Array (IProp r i)
        }



config :: { toLabel :: a -> String } -> Config a r i
config { toLabel } =
    Config
        { selected = Nothing
        , onChange = Nothing
        , toLabel = toLabel
        , additionalAttributes = []
        }



setSelected :: Maybe a -> Config a r i -> Config a r i
setSelected selected (Config config_) =
    Config { config_ | selected = selected }



setOnChange :: (a -> r i) -> Config a r i -> Config a r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



setAttributes :: Array (IProp r i) -> Config a r i -> Config a r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



chipSet :: Config a r i -> Array (Chip a r i) -> Html r i
chipSet ((Config { selected, onChange, toLabel, additionalAttributes }) as config_) chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetChoiceCs :: gridRole :: additionalAttributes)
        (Array.map (chip selected onChange toLabel) chips)


chip :: Maybe a -> Maybe (a -> r i) -> (a -> String) -> Chip a r i -> Html r i
chip selected onChange toLabel (Chip ((Chip.Config { additionalAttributes }) as config_) value) =
    Html.div [ HP.class_ mdc_touch_target_wrapper ]
        [ Html.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , selectedProp (Just value == selected)
                , interactionHandler (Maybe.map ((#) value) onChange)
                ]
                ++ additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt (toLabel value)
                ]
            )
        ]


chipSetCs :: Html.Attribute r i
chipSetCs =
    HP.class_ mdc_chip_set


chipSetChoiceCs :: Html.Attribute r i
chipSetChoiceCs =
    HP.class_ mdc_chip_set____choice


gridRole :: Html.Attribute r i
gridRole =
    Html.Attributes.attribute "role" "grid"


chipCs :: Maybe (Html.Attribute r i)
chipCs =
    Just (HP.class_ mdc_chip)


chipTextCs :: Html.Attribute r i
chipTextCs =
    HP.class_ mdc_chip__text


chipTouchCs :: Maybe (Html.Attribute r i)
chipTouchCs =
    Just (HP.class_ mdc_chip____touch)


chipPrimaryActionCs :: Html.Attribute r i
chipPrimaryActionCs =
    HP.class_ mdc_chip__primary_action


selectedProp :: Boolean -> Maybe (Html.Attribute r i)
selectedProp selected =
    Just (Html.Attributes.property "selected" (Encode.bool selected))


buttonRole :: Html.Attribute r i
buttonRole =
    Html.Attributes.attribute "role" "button"


rowRole :: Maybe (Html.Attribute r i)
rowRole =
    Just (Html.Attributes.attribute "role" "row")


gridcellRole :: Html.Attribute r i
gridcellRole =
    Html.Attributes.attribute "role" "gridcell"


interactionHandler :: Maybe r i -> Maybe (Html.Attribute r i)
interactionHandler r i =
    Maybe.map (Html.Events.on "MDCChip:interaction" << Decode.succeed) r i


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ HP.class_ mdc_chip__ripple ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        Html.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (Html.div [ HP.class_ mdc_chip__touch ] [])
