module HalogenMWC.ChipSet.Input where

import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Halogen (AttrName(..), ElemName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Chip.Input (Chip(..))
import HalogenMWC.Chip.Input as Chip
import Material.Classes.Chips (mdc_chip, mdc_chip____touch, mdc_chip__icon, mdc_chip__icon____leading, mdc_chip__icon____trailing, mdc_chip__primary_action, mdc_chip__ripple, mdc_chip__text, mdc_chip__touch, mdc_chip_set, mdc_chip_set____input, mdc_touch_target_wrapper)
import MaterialIconsFont.Classes (material_icons)
import Web.Event.Event (EventType(..))

chipSet :: forall w i. Array (IProp I.HTMLdiv i) -> Array (Tuple String (Chip i)) -> HH.HTML w i
chipSet additionalAttributes keyedChips =
  HH.keyed (ElemName "mdc-chip-set")
    ([ HP.class_ mdc_chip_set, HP.class_ mdc_chip_set____input, HP.attr (AttrName "role") "grid" ] <> additionalAttributes)
    (map (map chip) keyedChips)

chip :: forall w i. Chip i -> HH.HTML w i
chip (Chip config label) =
  HH.div [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element (ElemName "mdc-chip")
        ( [ HP.classes [ mdc_chip, mdc_chip____touch ]
          , HP.attr (AttrName "role") "row"
          ]
            <> Array.catMaybes
                [ removalHandler config
                ]
            <> config.additionalAttributes
        )
        ( Array.catMaybes
            [ Just rippleElt
            , leadingIconEltMaterialIcons config
            , Just $ primaryActionElt label
            , trailingIconEltMaterialIcons config
            ]
        )
    ]

removalHandler :: forall r i. Chip.Config i -> Maybe (IProp r i)
removalHandler config = map (HE.handler (EventType "MDCChip:removal")) config.onDelete

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

leadingIconEltMaterialIcons :: forall w i. Chip.Config i -> Maybe (HH.HTML w i)
leadingIconEltMaterialIcons config =
  map
    ( \iconName -> HH.i [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____leading ] ] [ HH.text iconName ]
    )
    config.leadingIcon

primaryActionElt :: forall w i. String -> HH.HTML w i
primaryActionElt label =
  HH.span
    [ HP.class_ mdc_chip__primary_action, HP.attr (AttrName "role") "gridcell", HP.tabIndex (-1) ]
    [ textElt label, touchElt ]

textElt :: forall w i. String -> HH.HTML w i
textElt label = HH.span [ HP.class_ mdc_chip__text, HP.attr (AttrName "role") "button" ] [ HH.text label ]

touchElt :: forall w i. HH.HTML w i
touchElt = HH.div [ HP.class_ mdc_chip__touch ] []

trailingIconEltMaterialIcons :: forall w i. Chip.Config i -> Maybe (HH.HTML w i)
trailingIconEltMaterialIcons config =
  map
    ( const
        $ HH.i
            [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____trailing ]
            , HP.tabIndex (-1)
            , HP.attr (AttrName "role") "button"
            ]
            [ HH.text (Maybe.fromMaybe "cancel" config.trailingIcon) ]
    )
    config.onDelete
