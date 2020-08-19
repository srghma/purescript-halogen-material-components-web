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
    Config { additionalAttributes = [] }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



drawer :: Config r i -> Array (Html r i) -> Html r i
drawer (Config { additionalAttributes }) nodes =
    Html.div
        (Array.filterMap identity [ rootCs ] ++ additionalAttributes)
        nodes



content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    Html.div (HP.class_ mdc_drawer__content :: attributes) nodes



header :: Array (IProp r i) -> Array (Html r i) -> Html r i
header additionalAttributes nodes =
    Html.div (HP.class_ mdc_drawer__header :: additionalAttributes) nodes



title :: Html.Attribute r i
title =
    HP.class_ mdc_drawer__title



subtitle :: Html.Attribute r i
subtitle =
    HP.class_ mdc_drawer__subtitle


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_drawer)
