module HalogenMWC.Fab.Extended where

import Halogen (ElemName(..), PropName(..))
import Material.Classes.Fab (mdc_fab, mdc_fab____exited, mdc_fab____extended, mdc_fab__icon, mdc_fab__label, mdc_fab__ripple)
import MaterialIconsFont.Classes (material_icons)
import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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

fab :: forall w i. Config i -> String -> HH.HTML w i
fab config label =
  HH.element (ElemName "mdc-fab")
    ( Array.catMaybes
        [ Just $ HP.classes
            $ [ mdc_fab, mdc_fab____extended ]
            <> if config.exited then [ mdc_fab____exited ] else []
        , Just $ HP.prop (PropName "tabIndex") 0
        , map HE.onClick config.onClick
        ]
        <> config.additionalAttributes
    )
    ( Array.catMaybes
        [ Just $ rippleElt
        , leadingIconElt config
        , Just $ labelElt label
        , trailingIconElt config
        ]
    )

rippleElt :: forall w i. HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_fab__ripple ] []

leadingIconElt :: forall w i. Config i -> Maybe (HH.HTML w i)
leadingIconElt config = case config.icon, config.trailingIcon of
  Just iconName, false ->
    Just
      ( HH.span
          [ HP.class_ material_icons, HP.class_ mdc_fab__icon ]
          [ HH.text iconName ]
      )
  _, _ -> Nothing

labelElt :: forall w i. String -> HH.HTML w i
labelElt label = HH.span [ HP.class_ mdc_fab__label ] [ HH.text label ]

trailingIconElt :: forall w i. Config i -> Maybe (HH.HTML w i)
trailingIconElt config = case config.icon, config.trailingIcon of
  Just iconName, true -> Just (HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ] [ HH.text iconName ])
  _, _ -> Nothing
