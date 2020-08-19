module HalogenMWC.Drawer.Modal where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { open :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClose :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { open: false
  , additionalAttributes: []
  , onClose: Nothing
  }

drawer :: Config r i -> Array (HH.HTML w i) -> HH.HTML w i
drawer (config_@{ additionalAttributes }) nodes =
  HH.element "mdc-drawer"
    ( Array.filterMap identity
        [ rootCs
        , modalCs
        , openProp config_
        , closeHandler config_
        ]
        <> additionalAttributes
    )
    nodes

content :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
content attributes nodes = HH.div ([ HP.class_ mdc_drawer__content ] <> attributes) nodes

header :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
header additionalAttributes nodes = HH.div ([ HP.class_ mdc_drawer__header ] <> additionalAttributes) nodes

title :: IProp r i
title = HP.class_ mdc_drawer__title

subtitle :: IProp r i
subtitle = HP.class_ mdc_drawer__subtitle

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_drawer)

modalCs :: Maybe (IProp r i)
modalCs = Just (HP.class_ mdc_drawer____modal)

openProp :: Config r i -> Maybe (IProp r i)
openProp { open } = Just (HP.prop "open" (Encode.bool open))

closeHandler :: Config r i -> Maybe (IProp r i)
closeHandler { onClose } = map (HH.Events.on "MDCDrawer:close" << Decode.succeed) onClose

scrim :: Array (IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
scrim additionalAttributes nodes = HH.div ([ HP.class_ mdc_drawer_scrim ] <> additionalAttributes) nodes
