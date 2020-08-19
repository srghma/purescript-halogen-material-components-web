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


















chipSet :: Config a r i -> Array (Chip a r i) -> Html r i
chipSet ((Config { selected, onChange, toLabel, additionalAttributes }) as config_) chips =
    HH.node "mdc-chip-set"
        ([ chipSetCs, chipSetChoiceCs, gridRole] <> additionalAttributes)
        (Array.map (chip selected onChange toLabel) chips)


chip :: Maybe a -> Maybe (a -> r i) -> (a -> String) -> Chip a r i -> Html r i
chip selected onChange toLabel (Chip ({ additionalAttributes } as config_) value) =
    HH.div [ HP.class_ mdc_touch_target_wrapper ]
        [ HH.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , selectedProp (Just value == selected)
                , interactionHandler (Maybe.map ((#) value) onChange)
                ]
                <> additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt (toLabel value)
                ]
            )
        ]


chipSetCs :: HH.Attribute r i
chipSetCs =
    HP.class_ mdc_chip_set


chipSetChoiceCs :: HH.Attribute r i
chipSetChoiceCs =
    HP.class_ mdc_chip_set____choice


gridRole :: HH.Attribute r i
gridRole =
    HH.Attributes.attribute "role" "grid"


chipCs :: Maybe (HH.Attribute r i)
chipCs =
    Just (HP.class_ mdc_chip)


chipTextCs :: HH.Attribute r i
chipTextCs =
    HP.class_ mdc_chip__text


chipTouchCs :: Maybe (HH.Attribute r i)
chipTouchCs =
    Just (HP.class_ mdc_chip____touch)


chipPrimaryActionCs :: HH.Attribute r i
chipPrimaryActionCs =
    HP.class_ mdc_chip__primary_action


selectedProp :: Boolean -> Maybe (HH.Attribute r i)
selectedProp selected =
    Just (HH.Attributes.property "selected" (Encode.bool selected))


buttonRole :: HH.Attribute r i
buttonRole =
    HH.Attributes.attribute "role" "button"


rowRole :: Maybe (HH.Attribute r i)
rowRole =
    Just (HH.Attributes.attribute "role" "row")


gridcellRole :: HH.Attribute r i
gridcellRole =
    HH.Attributes.attribute "role" "gridcell"


interactionHandler :: Maybe r i -> Maybe (HH.Attribute r i)
interactionHandler r i =
    Maybe.map (HH.Events.on "MDCChip:interaction" << Decode.succeed) r i


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_chip__ripple ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt { icon } =
    Maybe.map
        (\iconName ->
            HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        HH.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (HH.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (HH.div [ HP.class_ mdc_chip__touch ] [])
