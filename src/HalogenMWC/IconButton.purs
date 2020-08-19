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



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify an icon button's HTML5 arial-label attribute
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }



iconButton :: Config r i -> String -> Html r i
iconButton ((Config { additionalAttributes }) as config_) iconName =
    Html.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , materialIconsCs
            , tabIndexProp
            , clickHandler config_
            ]
            ++ additionalAttributes
        )
        [ text iconName ]



custom :: Config r i -> Array (Html r i) -> Html r i
custom ((Config { additionalAttributes }) as config_) nodes =
    Html.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , tabIndexProp
            , clickHandler config_
            ]
            ++ additionalAttributes
        )
        nodes


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (HP.class_ mdc_icon_button)


materialIconsCs :: Maybe (Html.Attribute r i)
materialIconsCs =
    Just (HP.class_ material_icons)


tabIndexProp :: Maybe (Html.Attribute r i)
tabIndexProp =
    Just (Html.Attributes.tabindex 0)


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick
