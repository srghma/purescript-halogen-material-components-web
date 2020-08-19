module HalogenMWC.Menu
    ( Config, config




    , menu, surfaceAnchor
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
        , quickOpen :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }



config :: Config r i
config =
    Config
        { open = False
        , quickOpen = False
        , additionalAttributes = []
        , onClose = Nothing
        }























menu :: Config r i -> Array (Html r i) -> Html r i
menu (config_@{ additionalAttributes }) nodes =
    HH.node "mdc-menu"
        (Array.filterMap identity
            [ rootCs
            , openProp config_
            , quickOpenProp config_
            , closeHandler config_
            ]
            <> additionalAttributes
        )
        nodes



surfaceAnchor :: HH.Attribute r i
surfaceAnchor =
    HP.class_ mdc_menu_surface____anchor


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-menu mdc-menu-surface")


openProp :: Config r i -> Maybe (HH.Attribute r i)
openProp { open } =
    Just (HH.Attributes.property "open" (Encode.bool open))


quickOpenProp :: Config r i -> Maybe (HH.Attribute r i)
quickOpenProp { quickOpen } =
    Just (HH.Attributes.property "quickOpen" (Encode.bool quickOpen))


closeHandler :: Config r i -> Maybe (HH.Attribute r i)
closeHandler { onClose } =
    Maybe.map (HH.Events.on "MDCMenu:close" << Decode.succeed) onClose
