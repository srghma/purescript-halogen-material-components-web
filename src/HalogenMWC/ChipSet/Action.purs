module HalogenMWC.ChipSet.Action where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

chipSet :: Array (IProp r i) -> Array (Chip r i) -> HH.HTML w i
chipSet additionalAttributes chips =
  HH.element "mdc-chip-set"
    ( [ HP.classes [ mdc_chip_set, mdc_chip_set____action ]
      , HP.attr "role" "grid"
      ]
        <> additionalAttributes
    )
    (map chip chips)

chip :: Chip r i -> HH.HTML w i
chip (Chip config_ label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element "mdc-chip"
        ( Array.catMaybes
            [ HP.class_ mdc_chip
            , HP.class_ mdc_chip____touch
            , HP.attr "role" "row"
            , interactionHandler config_
            ]
            <> config_.additionalAttributes
        )
        ( Array.catMaybes
            [ rippleElt
            , leadingIconElt config_
            , primaryActionElt label
            ]
        )
    ]

interactionHandler :: Chip.Config r i -> Maybe (IProp r i)
interactionHandler { onClick } = map (HH.Events.on "MDCChip:interaction" <<< Decode.succeed) onClick

rippleElt :: Maybe (HH.HTML w i)
rippleElt = Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (HH.HTML w i)
leadingIconElt config =
  map
    ( \iconName ->
        HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
          [ HH.text iconName ]
    )
    config.icon

primaryActionElt :: String -> Maybe (HH.HTML w i)
primaryActionElt label =
  Just
    $ HH.span [ HP.class_ mdc_chip__primary_action, HP.attr "role" "gridcell" ]
        (Array.catMaybes [ HH.textElt label, touchElt ])

textElt :: String -> Maybe (HH.HTML w i)
textElt label = Just (HH.span [ HP.class_ mdc_chip__text, HP.attr "role" "button" ] [ HH.text label ])

touchElt :: Maybe (HH.HTML w i)
touchElt = Just (HH.div [ HP.class_ mdc_chip__touch ] [])
