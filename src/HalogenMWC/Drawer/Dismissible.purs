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
    Html.node "mdc-drawer"
        (Array.filterMap identity
            [ rootCs
            , dismissibleCs
            , openProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
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


dismissibleCs :: Maybe (Html.Attribute r i)
dismissibleCs =
    Just (HP.class_ mdc_drawer____dismissible)


openProp :: Config r i -> Maybe (Html.Attribute r i)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


closeHandler :: Config r i -> Maybe (Html.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDrawer:close" << Decode.succeed) onClose



appContent :: Html.Attribute r i
appContent =
    HP.class_ mdc_drawer_app_content
