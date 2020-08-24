module HalogenMWC.ChipSet.Action where

import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Chips
import HalogenMWC.Chip.Action

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
chip (Chip config label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element "mdc-chip"
        ( [ HP.classes [ mdc_chip, mdc_chip____touch ]
          , HP.attr "role" "row"
          -- | , interactionHandler config
          ]
          <> config.additionalAttributes
        )
        ( [ HH.div
            [ HP.class_ mdc_chip__ripple ]
            (  [ rippleElt ]
            <> leadingIconElt config.icon
            <> [ primaryActionElt label ]
            )
          ]
        )
    ]

rippleElt :: HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

-- | interactionHandler :: Config r i -> Maybe (IProp r i)
-- | interactionHandler { onClick } = map (HH.Events.on "MDCChip:interaction" <<< Decode.succeed) onClick

leadingIconElt :: Maybe String -> Array (HH.HTML w i)
leadingIconElt Nothing = []
leadingIconElt (Just iconName) =
  HH.i
  [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____leading ]
  ]
  [ HH.text iconName
  ]

primaryActionElt :: String -> HH.HTML w i
primaryActionElt label =
  HH.span
    [ HP.class_ mdc_chip__primary_action
    , HP.attr "role" "gridcell"
    ]
    [ textElt label
    , HH.div
      [ HP.class_ mdc_chip__touch ]
      []
    ]

textElt :: String -> HH.HTML w i
textElt label =
  HH.span
  [ HP.class_ mdc_chip__text
  , HP.attr "role" "button"
  ]
  [ HH.text label ]
