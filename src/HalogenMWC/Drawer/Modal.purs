module HalogenMWC.Drawer.Modal
    ( Config, config



    , drawer, content
    , scrim
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



setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }



setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



drawer :: Config r i -> Array (Html r i) -> Html r i
drawer ((Config { additionalAttributes }) as config_) nodes =
    HH.node "mdc-drawer"
        (Array.filterMap identity
            [ rootCs
            , modalCs
            , openProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        nodes



content :: Array (IProp r i) -> Array (Html r i) -> Html r i
content attributes nodes =
    HH.div (HP.class_ mdc_drawer__content :: attributes) nodes



header :: Array (IProp r i) -> Array (Html r i) -> Html r i
header additionalAttributes nodes =
    HH.div (HP.class_ mdc_drawer__header :: additionalAttributes) nodes



title :: HH.Attribute r i
title =
    HP.class_ mdc_drawer__title



subtitle :: HH.Attribute r i
subtitle =
    HP.class_ mdc_drawer__subtitle


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_drawer)


modalCs :: Maybe (HH.Attribute r i)
modalCs =
    Just (HP.class_ mdc_drawer____modal)


openProp :: Config r i -> Maybe (HH.Attribute r i)
openProp (Config { open }) =
    Just (HH.Attributes.property "open" (Encode.bool open))


closeHandler :: Config r i -> Maybe (HH.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (HH.Events.on "MDCDrawer:close" << Decode.succeed) onClose



scrim :: Array (IProp r i) -> Array (Html r i) -> Html r i
scrim additionalAttributes nodes =
    HH.div (HP.class_ mdc_drawer_scrim :: additionalAttributes) nodes
