module HalogenMWC.ChipSet.Input where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Input
import HalogenMWC.Chip.Input as Chip

chipSet :: Array (IProp r i) -> Array (Tuple String (Chip r i)) -> HH.HTML w i
chipSet additionalAttributes keyedChips =
  HH.keyed "mdc-chip-set"
    ([ HP.class_ mdc_chip_set, HP.class_ mdc_chip_set____input, HP.attr "role" "grid" ] <> additionalAttributes)
    (map (Tuple.mapSecond chip) keyedChips)

chip :: Chip r i -> HH.HTML w i
chip (Chip config_ label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element "mdc-chip"
        ( Array.catMaybes
            [ HP.class_ mdc_chip
            , HP.class_ mdc_chip____touch
            , HP.attr "role" "row"
            , removalHandler config_
            ]
            <> config_.additionalAttributes
        )
        ( Array.catMaybes
            [ rippleElt
            , leadingIconElt config_
            , primaryActionElt label
            , trailingIconElt config_
            ]
        )
    ]

tabIndexProp :: Int -> IProp r i
tabIndexProp tabIndex = HP.prop "tabIndex" tabIndex

removalHandler :: Chip.Config r i -> Maybe (IProp r i)
removalHandler { onDelete } = map (HH.Events.on "MDCChip:removal" << Decode.succeed) onDelete

rippleElt :: Maybe (HH.HTML w i)
rippleElt = Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (HH.HTML w i)
leadingIconElt { leadingIcon } =
  map
    ( \iconName ->
        HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
          [ text iconName ]
    )
    leadingIcon

primaryActionElt :: String -> Maybe (HH.HTML w i)
primaryActionElt label =
  Just
    $ HH.span [ HP.class_ mdc_chip__primary_action, HP.attr "role" "gridcell", tabIndexProp - 1 ]
        (Array.catMaybes [ textElt label, touchElt ])

textElt :: String -> Maybe (HH.HTML w i)
textElt label = Just (HH.span [ HP.class_ mdc_chip__text, HP.attr "role" "button" ] [ text label ])

touchElt :: Maybe (HH.HTML w i)
touchElt = Just (HH.div [ HP.class_ mdc_chip__touch ] [])

trailingIconElt :: Chip.Config r i -> Maybe (HH.HTML w i)
trailingIconElt { trailingIcon, onDelete } =
  if onDelete /= Nothing then
    Just
      $ HH.i
          [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--trailing"
          , tabIndexProp - 1
          , HP.attr "role" "button"
          ]
          [ text (Maybe.fromMaybe "cancel" trailingIcon) ]
  else
    Nothing
