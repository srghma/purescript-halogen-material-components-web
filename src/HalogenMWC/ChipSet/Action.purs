module HalogenMWC.ChipSet.Action (chipSet) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Action as Chip

chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
  HH.node "mdc-chip-set"
    ( [ HP.classes [ mdc_chip_set, mdc_chip_set____action ]
      , HH.Attributes.attribute "role" "grid"
      ]
        <> additionalAttributes
    )
    (Array.map chip chips)

chip :: Chip r i -> Html r i
chip (Chip config_ label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.node "mdc-chip"
        ( Array.filterMap identity
            [ HP.class_ mdc_chip
            , HP.class_ mdc_chip____touch
            , HH.Attributes.attribute "role" "row"
            , interactionHandler config_
            ]
            <> config_.additionalAttributes
        )
        ( Array.filterMap identity
            [ rippleElt
            , leadingIconElt config_
            , primaryActionElt label
            ]
        )
    ]

interactionHandler :: Chip.Config r i -> Maybe (HH.Attribute r i)
interactionHandler { onClick } = map (HH.Events.on "MDCChip:interaction" << Decode.succeed) onClick

rippleElt :: Maybe (Html r i)
rippleElt = Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt config =
  map
    ( \iconName ->
        HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
          [ text iconName ]
    )
    config.icon

primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
  Just
    $ HH.span [ HP.class_ mdc_chip__primary_action, HH.Attributes.attribute "role" "gridcell" ]
        (Array.filterMap identity [ textElt label, touchElt ])

textElt :: String -> Maybe (Html r i)
textElt label = Just (HH.span [ HP.class_ mdc_chip__text, HH.Attributes.attribute "role" "button" ] [ text label ])

touchElt :: Maybe (Html r i)
touchElt = Just (HH.div [ HP.class_ mdc_chip__touch ] [])
