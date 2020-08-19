module HalogenMWC.IconButton
    ( Config, config
    
    
    
    
    , iconButton
    , custom
    ) where

{-| Icon buttons allow users to take actions and make choices with a single
tap.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Icon Button](#icon-button)
  - [Disabled Icon Button](#disabled-icon-button)
  - [Labeled Icon Button](#labeled-icon-button)
  - [Focus an Icon Button](#focus-an-icon-button)


# Resources

  - [Demo: Icon buttons](https://aforemny.github.io/material-components-web-elm/#icon-button)
  - [Material Design Guidelines: Toggle buttons](https://material.io/go/design-buttons#toggle-button)
  - [MDC Web: Icon Button](https://github.com/material-components/material-components-web/tree/master/packages/mdc-icon-button)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-icon-button#sass-mixins)


# Basic Usage

    import HalogenMWC.IconButton as IconButton

    data Msg
        = Clicked

    main =
        IconButton.iconButton
            (IconButton.config |> IconButton.setOnClick Clicked)
            "favorite"


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setDisabled
@docs setLabel
@docs setAttributes


# Icon Button

@docs iconButton


# Disabled Icon Button

To disable an icon button its `setDisabled` configuration option to
`True`. Disabled icon buttons cannot be interacted with and have no visual
interaction effect.

    IconButton.iconButton
        (IconButton.config |> IconButton.setDisabled True)
        "favorite"


# Labeled Icon Button

To set the HTML attribute `arial-label` of a icon button, use its `setLabel`
configuration option.

    IconButton.iconButton
        (IconButton.config
            |> IconButton.setLabel (Just "Add to favorites")
        )
        "favorite"


# Variant: Custom Icon Button

@docs custom


# Focus an Icon Button

You may programatically focus an icon button by assigning an id attribute to it
and use `Browser.Dom.focus`.

    IconButton.iconButton
        (IconButton.config
            |> IconButton.setAttributes
                [ Html.Attributes.id "my-icon-button" ]
        )
        "wifi"

-}

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
    Just (class "mdc-icon-button")


materialIconsCs :: Maybe (Html.Attribute r i)
materialIconsCs =
    Just (class "material-icons")


tabIndexProp :: Maybe (Html.Attribute r i)
tabIndexProp =
    Just (Html.Attributes.tabindex 0)


clickHandler :: Config r i -> Maybe (Html.Attribute r i)
clickHandler (Config { onClick }) =
    Maybe.map Html.Events.onClick onClick
