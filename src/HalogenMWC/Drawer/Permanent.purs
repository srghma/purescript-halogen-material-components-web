module HalogenMWC.Drawer.Permanent where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { additionalAttributes :: Array (IProp r i) }

defaultConfig :: Config r i
defaultConfig = { additionalAttributes: [] }

drawer :: Config r i -> Array (HH.HTML w i) -> HH.HTML w i
drawer { additionalAttributes } nodes =
  HH.div
    (Array.catMaybes [ rootCs ] <> additionalAttributes)
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
