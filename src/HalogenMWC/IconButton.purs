module HalogenMWC.IconButton
    ( Config, config




    , iconButton
    , custom
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


import HalogenMWC.IconButton.Internal (Config(..))



type Config r i =
    Material.IconButton.Internal.Config r i



config :: Config r i
config =
    Config
        { disabled = False
        , label = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        }







{-| Specify an icon button's HTML5 arial-label attribute
-}















iconButton :: Config r i -> String -> Html r i
iconButton ((Config { additionalAttributes }) as config_) iconName =
    HH.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , materialIconsCs
            , tabIndexProp
            , clickHandler config_
            ]
            <> additionalAttributes
        )
        [ text iconName ]



custom :: Config r i -> Array (Html r i) -> Html r i
custom ((Config { additionalAttributes }) as config_) nodes =
    HH.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , tabIndexProp
            , clickHandler config_
            ]
            <> additionalAttributes
        )
        nodes


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_icon_button)


materialIconsCs :: Maybe (HH.Attribute r i)
materialIconsCs =
    Just (HP.class_ material_icons)


tabIndexProp :: Maybe (HH.Attribute r i)
tabIndexProp =
    Just (HH.Attributes.tabindex 0)


clickHandler :: Config r i -> Maybe (HH.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map HH.Events.onClick onClick
