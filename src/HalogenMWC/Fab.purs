module HalogenMWC.Fab where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { mini :: Boolean
    , exited :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClick :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { mini: false
  , exited: false
  , onClick: Nothing
  , additionalAttributes: []
  }

fab :: Config r i -> String -> HH.HTML w i
fab (config_@{ additionalAttributes }) iconName =
  HH.element "mdc-fab"
    ( Array.catMaybes
        [ rootCs
        , miniCs config_
        , exitedCs config_
        , clickHandler config_
        , tabIndexProp 0
        ]
        <> additionalAttributes
    )
    [ rippleElt
    , iconElt iconName
    ]

tabIndexProp :: Int -> Maybe (IProp r i)
tabIndexProp tabIndex = Just (HP.prop "tabIndex" tabIndex)

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_fab)

miniCs :: Config r i -> Maybe (IProp r i)
miniCs { mini } =
  if mini then
    Just (HP.class_ mdc_fab____mini)
  else
    Nothing

exitedCs :: Config r i -> Maybe (IProp r i)
exitedCs { exited } =
  if exited then
    Just (HP.class_ mdc_fab____exited)
  else
    Nothing

rippleElt :: HH.HTML w i
rippleElt = HH.div [ HP.class_ mdc_fab__ripple ] []

iconElt :: String -> HH.HTML w i
iconElt iconName = HH.span [ HP.class_ material_icons, HP.class_ mdc_fab__icon ] [ HH.text iconName ]

clickHandler :: Config r i -> Maybe (IProp r i)
clickHandler { onClick } = map HH.Events.onClick onClick
