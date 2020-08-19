module HalogenMWC.Drawer.Dismissible
    ( Config, config



    , drawer, content
    , appContent
    , header, title, subtitle
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }



config :: Config r i
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }


















drawer :: Config r i -> Array (Html r i) -> Html r i
drawer ((Config { additionalAttributes }) as config_) nodes =
    HH.node "mdc-drawer"
        (Array.filterMap identity
            [ rootCs
            , dismissibleCs
            , openProp config_
            , closeHandler config_
            ]
            <> additionalAttributes
        )
        nodes



content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    HH.div ([ HP.class_ mdc_drawer__content ] <> attributes) nodes



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


dismissibleCs :: Maybe (HH.Attribute r i)
dismissibleCs =
    Just (HP.class_ mdc_drawer____dismissible)


openProp :: Config r i -> Maybe (HH.Attribute r i)
openProp (Config { open }) =
    Just (HH.Attributes.property "open" (Encode.bool open))


closeHandler :: Config r i -> Maybe (HH.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (HH.Events.on "MDCDrawer:close" << Decode.succeed) onClose



appContent :: HH.Attribute r i
appContent =
    HP.class_ mdc_drawer_app_content
