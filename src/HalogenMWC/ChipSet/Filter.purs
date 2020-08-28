module HalogenMWC.ChipSet.Filter where

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
import HalogenMWC.Chip.Filter as Chip
import HalogenMWC.Chip.Filter
import Halogen.SVG.Elements as Halogen.SVG.Elements
import Halogen.SVG.Attributes as Halogen.SVG.Attributes
import Material.Classes.Chips

chipSet :: forall w i . Array (IProp I.HTMLdiv i) -> Array (Chip i) -> HH.HTML w i
chipSet additionalAttributes chips =
  HH.element (ElemName "mdc-chip-set")
    ( [ HP.classes [ mdc_chip_set, mdc_chip_set____filter ]
      , HP.attr (AttrName "role") "grid"
      ] <> additionalAttributes
    )
    (map chip chips)

chip :: forall w i . Chip i -> HH.HTML w i
chip (Chip config label) =
  HH.div
    [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element (ElemName "mdc-chip")
        ( Array.catMaybes
            [ HP.classes [ mdc_chip, mdc_chip____touch ]
            , HP.attr (AttrName "role") "row"
            , selectedProp config
            , interactionHandler config
            ]
            <> config.additionalAttributes
        )
        ( Array.catMaybes
            [ rippleElt
            , leadingIconElt config
            , checkmarkElt
            , primaryActionElt label
            ]
        )
    ]

selectedProp :: forall r i . Chip.Config i -> IProp r i
selectedProp config = HP.prop (PropName "selected") config.selected

interactionHandler :: forall r i . Chip.Config i -> Maybe (IProp r i)
interactionHandler config = map (HE.handler (EventType "MDCChip:interaction")) config.onChange

rippleElt :: forall w i . HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

leadingIconElt :: forall w i . Chip.Config i -> Maybe (HH.HTML w i)
leadingIconElt config =
  map
    ( \iconName ->
        HH.i
          [ HP.classes [ material_icons, mdc_chip__icon, mdc_chip__icon____leading ]
          ]
          [ HH.text iconName ]
    )
    config.icon

checkmarkElt :: forall w i . HH.HTML w i
checkmarkElt =
  HH.div
    [ HP.class_ mdc_chip__checkmark ]
    [ Halogen.SVG.Elements.svg
      [ Halogen.SVG.Attributes.class_ mdc_chip__checkmark_svg
      , HP.attr (AttrName "viewBox") "-2 -3 30 30"
      ]
      [ Halogen.SVG.Elements.path
          [ Halogen.SVG.Attributes.class_ mdc_chip__checkmark_path
          , HP.attr (AttrName "fill") "none"
          , Halogen.SVG.Attributes.stroke "black"
          , HP.attr (AttrName "d") "M1.73,12.91 8.1,19.28 22.79,4.59"
          ]
          []
      ]
    ]

primaryActionElt :: forall w i . String -> HH.HTML w i
primaryActionElt label =
  HH.span
  [ HP.class_ mdc_chip__primary_action, HP.attr (AttrName "role") "gridcell" ]
  [ textElt label, touchElt ]

textElt :: forall w i . String -> HH.HTML w i
textElt label = HH.span [ HP.class_ mdc_chip__text, HP.attr (AttrName "role") "button" ] [ HH.text label ]

touchElt :: forall w i . HH.HTML w i
touchElt = HH.div [ HP.class_ mdc_chip__touch ] []
