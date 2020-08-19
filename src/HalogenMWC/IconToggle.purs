module HalogenMWC.IconToggle
    ( Config, config
    
    
    
    
    
    , iconToggle
    ) where

{-| Icon toggles allow users to take actions and make choices with a single
tap.


# Table of Contents

  - [Resources](#resources)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Basic Usage](#basic-usage)
  - [Icon Toggle](#icon-toggle)
  - [On Icon Toggle](#on-icon-toggle)
  - [Disabled Icon Toggle](#disabled-icon-toggle)
  - [Labeled Icon Toggle](#labeled-icon-toggle)
  - [Focus an Icon Toggle](#focus-an-icon-toggle)


# Resources

  - [Demo: Icon buttons](https://aforemny.github.io/material-components-web-elm/#icon-button)
  - [Material Design Guidelines: Toggle buttons](https://material.io/go/design-buttons#toggle-button)
  - [MDC Web: Icon Button](https://github.com/material-components/material-components-web/tree/master/packages/mdc-icon-button)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-icon-button#sass-mixins)


# Basic Usage

    import HalogenMWC.IconToggle as IconToggle

    data Msg
        = Clicked

    main =
        IconToggle.iconToggle
            (IconToggle.config
                |> IconToggle.setOn True
                |> IconToggle.setOnChange Clicked
            )
            { offIcon = "favorite_outlined"
            , onIcon = "favorite"
            }


# Configuration

@docs Config, config


## Configuration Options

@docs setOnChange
@docs setOn
@docs setDisabled
@docs setLabel
@docs setAttributes


# Icon Toggle

Icon toggles are a variant of [icon buttons](Material-IconButton) that change
the icon when their state changes.

    IconToggle.iconToggle
        (IconToggle.config |> IconToggle.setOn True)
        { offIcon = "favorite_border"
        , onIcon = "favorite"
        }

@docs iconToggle


# On Icon Toggle

To set an icon toggle to its on state its `setOn` configuration option to
`True`.

    IconToggle.iconToggle
        (IconToggle.config |> IconToggle.setOn True)
        { offIcon = "favorite_border"
        , onIcon = "favorite"
        }


# Disabled Icon Toggle

To disable an icon toggle its `setDisabled` configuration option to
`True`.
Disabled icon buttons cannot be interacted with and have no visual interaction
effect.

    IconToggle.iconToggle
        (IconToggle.config |> IconToggle.setDisabled True)
        { offIcon = "favorite_border"
        , onIcon = "favorite"
        }


# Labeled Icon Toggle

To set the HTML5 `arial-label` attribute of an icon toggle, use its `setLabel`
configuration option.

    IconToggle.iconToggle
        (IconToggle.config
            |> IconToggle.setLabel (Just "Add to favorites")
        )
        { offIcon = "favorite_border"
        , onIcon = "favorite"
        }


# Focus an Icon Toggle

You may programatically focus an icon toggle by assigning an id attribute to it
and use `Browser.Dom.focus`.

    IconToggle.iconToggle
        (IconToggle.config
            |> IconToggle.setAttributes
                [ Html.Attributes.id "my-icon-toggle" ]
        )
        { offIcon = "favorite_border"
        , onIcon = "favorite"
        }

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { on :: Boolean
        , disabled :: Boolean
        , label :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onChange :: Maybe r i
        }



config :: Config r i
config =
    Config
        { on = False
        , disabled = False
        , label = Nothing
        , additionalAttributes = []
        , onChange = Nothing
        }



setOn :: Boolean -> Config r i -> Config r i
setOn on (Config config_) =
    Config { config_ | on = on }



setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify the HTML5 aria-label attribute of an icon toggle
-}
setLabel :: Maybe String -> Config r i -> Config r i
setLabel label (Config config_) =
    Config { config_ | label = label }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: r i -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



iconToggle :: Config r i -> { onIcon :: String, offIcon :: String } -> Html r i
iconToggle ((Config { additionalAttributes }) as config_) { onIcon, offIcon } =
    Html.node "mdc-icon-button"
        (Array.filterMap identity
            [ rootCs
            , onProp config_
            , tabIndexProp
            , ariaHiddenAttr
            , ariaPressedAttr config_
            , ariaLabelAttr config_
            , changeHandler config_
            , disabledAttr config_
            ]
            ++ additionalAttributes
        )
        [ Html.i (Array.filterMap identity [ materialIconsCs, onIconCs ]) [ text onIcon ]
        , Html.i (Array.filterMap identity [ materialIconsCs, iconCs ]) [ text offIcon ]
        ]


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-icon-button")


onProp :: Config r i -> Maybe (Html.Attribute r i)
onProp (Config { on }) =
    Just (Html.Attributes.property "on" (Encode.bool on))


materialIconsCs :: Maybe (Html.Attribute r i)
materialIconsCs =
    Just (class "material-icons")


iconCs :: Maybe (Html.Attribute r i)
iconCs =
    Just (class "mdc-icon-button__icon")


onIconCs :: Maybe (Html.Attribute r i)
onIconCs =
    Just (class "mdc-icon-button__icon mdc-icon-button__icon--on")


tabIndexProp :: Maybe (Html.Attribute r i)
tabIndexProp =
    Just (Html.Attributes.tabindex 0)


ariaHiddenAttr :: Maybe (Html.Attribute r i)
ariaHiddenAttr =
    Just (Html.Attributes.attribute "aria-hidden" "true")


ariaPressedAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaPressedAttr (Config { on }) =
    Just
        (Html.Attributes.attribute "aria-pressed"
            (if on then
                "true"

             else
                "false"
            )
        )


ariaLabelAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaLabelAttr (Config { label }) =
    Maybe.map (Html.Attributes.attribute "aria-label") label


changeHandler :: Config r i -> Maybe (Html.Attribute r i)
changeHandler (Config { onChange }) =
    Maybe.map (Html.Events.on "MDCIconButtonToggle:change" << Decode.succeed)
        onChange


disabledAttr :: Config r i -> Maybe (Html.Attribute r i)
disabledAttr (Config { disabled }) =
    Just (Html.Attributes.disabled disabled)
