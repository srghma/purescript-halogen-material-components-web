module HalogenMWC.ChipSet.Action where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.Chips
import HalogenMWC.Chip.Action

chipSet :: forall w i . Array (IProp I.HTMLdiv i) -> Array (Chip i) -> HH.HTML w i
chipSet additionalAttributes chips =
  HH.element (ElemName "mdc-chip-set")
    ( [ HP.classes [ mdc_chip_set ]
      , HP.attr (AttrName "role") "grid"
      ]
      <> additionalAttributes
    )
    (map chip chips)

chip :: forall w i . Chip i -> HH.HTML w i
chip (Chip config label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element (ElemName "mdc-chip")
        ( [ HP.classes [ mdc_chip, mdc_chip____touch ]
          , HP.attr (AttrName "role") "row"
          ]
          <> Array.catMaybes
          [ interactionHandler config.onClick
          ]
          <> config.additionalAttributes
        )
        ( [ HH.div
            [ HP.class_ mdc_chip__ripple ]
            ( Array.catMaybes
              [ Just rippleElt
              , leadingIconElt config.icon
              , Just $ primaryActionElt label
              ]
            )
          ]
        )
    ]

rippleElt :: forall w i . HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

interactionHandler :: forall r i . Maybe (Event -> i) -> Maybe (IProp r i)
interactionHandler = map (HE.handler (EventType "MDCChip:interaction"))

leadingIconElt :: forall w i . Maybe String -> Maybe (HH.HTML w i)
leadingIconElt = map
  (\iconName ->
    HH.i
    [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____leading ]
    ]
    [ HH.text iconName
    ]
  )

primaryActionElt :: forall w i . String -> HH.HTML w i
primaryActionElt label =
  HH.span
    [ HP.class_ mdc_chip__primary_action
    , HP.attr (AttrName "role") "gridcell"
    ]
    [ textElt label
    , HH.div
      [ HP.class_ mdc_chip__touch ]
      []
    ]

textElt :: forall w i . String -> HH.HTML w i
textElt label =
  HH.span
  [ HP.class_ mdc_chip__text
  , HP.attr (AttrName "role") "button"
  ]
  [ HH.text label ]
