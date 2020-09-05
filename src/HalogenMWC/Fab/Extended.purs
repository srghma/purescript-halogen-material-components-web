module HalogenMWC.Fab.Extended where

import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen (ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Fab (fabMateiralIcons)
import Material.Classes.Fab (mdc_fab, mdc_fab____exited, mdc_fab____extended, mdc_fab__icon, mdc_fab__label, mdc_fab__ripple)
import MaterialIconsFont.Classes (material_icons)
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i
  = { icon :: Maybe String
    , trailingIcon :: Boolean
    , exited :: Boolean
    , onClick :: Maybe (MouseEvent -> i)
    , additionalAttributes :: Array (IProp I.HTMLbutton i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { icon: Nothing
  , trailingIcon: false
  , exited: false
  , onClick: Nothing
  , additionalAttributes: []
  }

fabMateiralIcons :: forall w i. Config i -> String -> HH.HTML w i
fabMateiralIcons config label =
  HH.element (ElemName "mdc-fab")
    ( Array.catMaybes
        [ Just $ HP.classes
            $ [ mdc_fab, mdc_fab____extended ]
            <> if config.exited then [ mdc_fab____exited ] else []
        , Just $ HP.tabIndex 0
        , map HE.onClick config.onClick
        ]
        <> config.additionalAttributes
    )
    ( Array.catMaybes
        [ Just $ rippleElt
        , leadingIconEltMaterialIcons config
        , Just $ labelElt label
        , trailingIconEltMaterialIcons config
        ]
    )

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_fab__ripple ] []

leadingIconEltMaterialIcons :: forall w i. Config i -> Maybe (HH.HTML w i)
leadingIconEltMaterialIcons config = case config.icon, config.trailingIcon of
  Just iconName, false ->
    Just
      ( HH.span
          [ HP.classes [ material_icons, mdc_fab__icon ] ]
          [ HH.text iconName ]
      )
  _, _ -> Nothing

labelElt :: forall w i. String -> HH.HTML w i
labelElt label = HH.span [ HP.class_ mdc_fab__label ] [ HH.text label ]

trailingIconEltMaterialIcons :: forall w i. Config i -> Maybe (HH.HTML w i)
trailingIconEltMaterialIcons config = case config.icon, config.trailingIcon of
  Just iconName, true -> Just (HH.span [ HP.classes [ material_icons, mdc_fab__icon ] ] [ HH.text iconName ])
  _, _ -> Nothing
