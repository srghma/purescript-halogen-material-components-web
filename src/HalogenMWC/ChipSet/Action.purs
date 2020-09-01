module HalogenMWC.ChipSet.Action where

import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes (material_icons)
import Web.Event.Event (Event, EventType(..))
import Data.Array as Array
import Halogen (AttrName(..), ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Chips (mdc_chip, mdc_chip____touch, mdc_chip__icon, mdc_chip__icon____leading, mdc_chip__primary_action, mdc_chip__ripple, mdc_chip__text, mdc_chip__touch, mdc_chip_set, mdc_touch_target_wrapper)
import HalogenMWC.Chip.Action (Chip(..))

chipSet :: forall w i. Array (IProp I.HTMLdiv i) -> Array (Chip i) -> HH.HTML w i
chipSet additionalAttributes chips =
  HH.element (ElemName "mdc-chip-set")
    ( [ HP.classes [ mdc_chip_set ]
      , HP.attr (AttrName "role") "grid"
      ]
        <> additionalAttributes
    )
    (map chip chips)

chip :: forall w i. Chip i -> HH.HTML w i
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
        ( Array.catMaybes
            [ Just rippleElt
            , leadingIconElt config.icon
            , Just $ primaryActionElt label
            ]
        )
    ]

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

interactionHandler :: forall r i. Maybe (Event -> i) -> Maybe (IProp r i)
interactionHandler = map (HE.handler (EventType "MDCChip:interaction"))

leadingIconElt :: forall w i. Maybe String -> Maybe (HH.HTML w i)
leadingIconElt =
  map
    ( \iconName ->
        HH.i
          [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____leading ]
          ]
          [ HH.text iconName
          ]
    )

primaryActionElt :: forall w i. String -> HH.HTML w i
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

textElt :: forall w i. String -> HH.HTML w i
textElt label =
  HH.span
    [ HP.class_ mdc_chip__text
    , HP.attr (AttrName "role") "button"
    ]
    [ HH.text label ]
