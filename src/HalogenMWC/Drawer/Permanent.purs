module HalogenMWC.Drawer.Permanent
    ( Config, config

    , drawer, content
    , header, title, subtitle
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
    = { additionalAttributes :: Array (IProp r i) }

config :: Config r i
config =
    { additionalAttributes = [] }

drawer :: Config r i -> Array (Html r i) -> Html r i
drawer { additionalAttributes } nodes =
    HH.div
        (Array.filterMap identity [ rootCs ] <> additionalAttributes)
        nodes

content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    HH.div ([HP.class_ mdc_drawer__content] <> attributes) nodes

header :: Array (IProp r i) -> Array (Html r i) -> Html r i
header additionalAttributes nodes =
    HH.div ([HP.class_ mdc_drawer__header] <> additionalAttributes) nodes

title :: HH.Attribute r i
title =
    HP.class_ mdc_drawer__title

subtitle :: HH.Attribute r i
subtitle =
    HP.class_ mdc_drawer__subtitle

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_drawer)
