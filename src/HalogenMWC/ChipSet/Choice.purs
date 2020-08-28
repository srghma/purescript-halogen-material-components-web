module HalogenMWC.ChipSet.Choice where

import HalogenMWC.Chip.Choice
import Material.Classes.Chips
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
import MaterialIconsFont.Classes
import HalogenMWC.Chip.Choice as Chip.Choice

type Config a i =
  { selected :: Maybe a
  , onChange :: Maybe (a -> i)
  , toLabel :: a -> String
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

defaultConfig :: forall a i . { toLabel :: a -> String } -> Config a i
defaultConfig { toLabel } =
  { selected: Nothing
  , onChange: Nothing
  , toLabel: toLabel
  , additionalAttributes: []
  }

chipSet :: forall a w i . Eq a => Config a i -> Array (Chip a i) -> HH.HTML w i
chipSet config chips =
  HH.element (ElemName "mdc-chip-set")
    ( [ HP.classes [ mdc_chip_set, mdc_chip_set____choice ]
      , HP.attr (AttrName "role") "grid"
      ] <> config.additionalAttributes
      )
    (map (chip config.selected config.onChange config.toLabel) chips)

chip :: forall a w i . Eq a => Maybe a -> Maybe (a -> i) -> (a -> String) -> Chip a i -> HH.HTML w i
chip selected onChange toLabel (Chip config value) =
  HH.div
    [ HP.class_ mdc_touch_target_wrapper ]
    [ HH.element (ElemName "mdc-chip")
        ( [ HP.classes [ mdc_chip, mdc_chip____touch ]
          , HP.attr (AttrName "role") "row"
          , HP.prop (PropName "selected") (Just value == selected)
          ]
          <> Array.catMaybes
          [ interactionHandler (map (\f _event -> f value) onChange)
          ]
          <> config.additionalAttributes
        )
        ( Array.catMaybes
            [ Just rippleElt
            , leadingIconElt config
            , Just $ primaryActionElt (toLabel value)
            ]
        )
    ]

buttonRole :: forall r i . IProp r i
buttonRole = HP.attr (AttrName "role") "button"

gridcellRole :: forall  r i . IProp r i
gridcellRole = HP.attr (AttrName "role") "gridcell"

interactionHandler :: forall i . Maybe (Event -> i) -> Maybe (IProp I.HTMLdiv i)
interactionHandler = map (HE.handler (EventType "MDCChip:interaction"))

rippleElt :: forall w i . HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_chip__ripple ] []

leadingIconElt :: forall w i . Chip.Choice.Config i -> Maybe (HH.HTML w i)
leadingIconElt config =
  map
    ( \iconName ->
        HH.i
          [ HP.classes [material_icons, mdc_chip__icon, mdc_chip__icon____leading] ]
          [ HH.text iconName ]
    )
    config.icon

primaryActionElt :: forall w i . String -> HH.HTML w i
primaryActionElt label = HH.span [ HP.class_ mdc_chip__primary_action, gridcellRole ] [ textElt label, touchElt ]

textElt :: forall w i . String -> HH.HTML w i
textElt label = HH.span [ HP.class_ mdc_chip__text, buttonRole ] [ HH.text label ]

touchElt :: forall w i . HH.HTML w i
touchElt = HH.div [ HP.class_ mdc_chip__touch ] []
