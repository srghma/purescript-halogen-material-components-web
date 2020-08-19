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



setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }



setQuickOpen :: Boolean -> Config r i -> Config r i
setQuickOpen quickOpen (Config config_) =
    Config { config_ | quickOpen = quickOpen }



setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



menu :: Config r i -> Array (Html r i) -> Html r i
menu ((Config { additionalAttributes }) as config_) nodes =
    HH.node "mdc-menu"
        (Array.filterMap identity
            [ rootCs
            , openProp config_
            , quickOpenProp config_
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        nodes



surfaceAnchor :: HH.Attribute r i
surfaceAnchor =
    HP.class_ mdc_menu_surface____anchor


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ "mdc-menu mdc-menu-surface")


openProp :: Config r i -> Maybe (HH.Attribute r i)
openProp (Config { open }) =
    Just (HH.Attributes.property "open" (Encode.bool open))


quickOpenProp :: Config r i -> Maybe (HH.Attribute r i)
quickOpenProp (Config { quickOpen }) =
    Just (HH.Attributes.property "quickOpen" (Encode.bool quickOpen))


closeHandler :: Config r i -> Maybe (HH.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (HH.Events.on "MDCMenu:close" << Decode.succeed) onClose
