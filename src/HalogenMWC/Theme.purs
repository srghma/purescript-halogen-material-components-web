module HalogenMWC.Theme
    ( primary, secondary, background
    , onPrimary, onSecondary, onSurface
    , primaryBg, secondaryBg, surface
    , textPrimaryOnBackground, textSecondaryOnBackground, textHintOnBackground
    , textDisabledOnBackground, textIconOnBackground
    , textPrimaryOnLight, textSecondaryOnLight, textHintOnLight
    , textDisabledOnLight, textIconOnLight
    , textPrimaryOnDark, textSecondaryOnDark, textHintOnDark
    , textDisabledOnDark, textIconOnDark
    ) where

{-| The Material Design color system can be used to create a color scheme that
reflects your brand or style.

Material Components Web use a theme comprised of a primary and a secondary
color. Those colors can be conveniently override via Sass (see below), and
[less conveniently via
CSS](https://github.com/material-components/material-components-web/tree/master/packages/mdc-theme#non-sass-customization).
They cannot be changed from Elm at all.

While this module defines attributes that mimic the CSS classes available by
MDC Web, it is recommended to use MDC Web's Sass API to do
themeing since it is a lot more flexible. I highly recommend you [check it
out](https://github.com/material-components/material-components-web/tree/master/packages/mdc-theme#sass-mixins-variables-and-functions)!


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Changing Theme via SASS](#chaging-theme-via-sass)
  - [Colors](#colors)
      - [Text Colors](#text-colors)
      - [Background Colors](#background-colors)
  - [Text Styles](#text-styles)
      - [Text Styles on Background](#text-styles-on-background)
      - [Text Styles on Light Background](#text-styles-on-light-background)
      - [Text Styles on Dark Background](#text-styles-on-dark-background)


# Resources

  - [Demo: Theme](https://aforemny.github.io/material-components-web-elm/#theme)
  - [Material Design Guidelines: Color](https://material.io/go/design-theming)
  - [MDC Web: Theme](https://github.com/material-components/material-components-web/tree/master/packages/mdc-theme)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-theme#sass-mixins-variables-and-functions)


# Basic Usage

    import HalogenMWC.Theme as Theme

    main =
        Html.span [ Theme.primary ] [ text "Primary color" ]


# Changing Theme via SASS

```scss
$mdc-theme-primary: #fcb8ab;
$mdc-theme-secondary: #feeae6;
$mdc-theme-on-primary: #442b2d;
$mdc-theme-on-secondary: #442b2d;

@import "@material/button/mdc-button";
```


# Colors


## Text Colors

@docs primary, secondary, background
@docs onPrimary, onSecondary, onSurface


## Background Colors

@docs primaryBg, secondaryBg, surface


# Text Styles

MDC Web use a system that defines five **text styles**. In addition to the
theme's background color, they can be used on either light or dark background.
The text styles are defined as follows:

Those text styles can be used on light background, dark background and on the
theme's background color whether it is light or dark.

  - **primary**: Used for most text
  - **secondary**: Used for text which is lower in the visual hierarchy
  - **hint**: Used for text hints, such as those in text fields and labels
  - **disabled**: Used for text in disabled components and content
  - **icon**: Used for icons

Please note that the primary and secondary text style _do not_ correspond to
the theme's primary or secondary colors.


## Text Styles on Background

@docs textPrimaryOnBackground, textSecondaryOnBackground, textHintOnBackground
@docs textDisabledOnBackground, textIconOnBackground


## Text Styles on Light Background

@docs textPrimaryOnLight, textSecondaryOnLight, textHintOnLight
@docs textDisabledOnLight, textIconOnLight


## Text Styles on Dark Background

@docs textPrimaryOnDark, textSecondaryOnDark, textHintOnDark
@docs textDisabledOnDark, textIconOnDark

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




primary :: Html.Attribute r i
primary =
    class "mdc-theme--primary"



secondary :: Html.Attribute r i
secondary =
    class "mdc-theme--secondary"



background :: Html.Attribute r i
background =
    class "mdc-theme--background"



surface :: Html.Attribute r i
surface =
    class "mdc-theme--surface"


{-| Sets the text color to the theme on-primary color

The theme's on-primary color is a text color that works best on a primary color
background.

-}
onPrimary :: Html.Attribute r i
onPrimary =
    class "mdc-theme--on-primary"


{-| Sets the text color to the theme on-secondary color

The theme's on-secondary color is a text color that works best on a secondary
color background.

-}
onSecondary :: Html.Attribute r i
onSecondary =
    class "mdc-theme--on-secondary"


{-| Sets the text color to the theme on-surface color

The theme's on-surface color is a text color that works best on a surface
color background.

-}
onSurface :: Html.Attribute r i
onSurface =
    class "mdc-theme--on-surface"



primaryBg :: Html.Attribute r i
primaryBg =
    class "mdc-theme--primary-bg"



secondaryBg :: Html.Attribute r i
secondaryBg =
    class "mdc-theme--secondary-bg"



textPrimaryOnLight :: Html.Attribute r i
textPrimaryOnLight =
    class "mdc-theme--text-primary-on-light"



textSecondaryOnLight :: Html.Attribute r i
textSecondaryOnLight =
    class "mdc-theme--text-secondary-on-light"



textHintOnLight :: Html.Attribute r i
textHintOnLight =
    class "mdc-theme--text-hint-on-light"



textDisabledOnLight :: Html.Attribute r i
textDisabledOnLight =
    class "mdc-theme--text-disabled-on-light"



textIconOnLight :: Html.Attribute r i
textIconOnLight =
    class "mdc-theme--text-icon-on-light"



textPrimaryOnDark :: Html.Attribute r i
textPrimaryOnDark =
    class "mdc-theme--text-primary-on-dark"



textSecondaryOnDark :: Html.Attribute r i
textSecondaryOnDark =
    class "mdc-theme--text-secondary-on-dark"



textHintOnDark :: Html.Attribute r i
textHintOnDark =
    class "mdc-theme--text-hint-on-dark"



textDisabledOnDark :: Html.Attribute r i
textDisabledOnDark =
    class "mdc-theme--text-disabled-on-dark"



textIconOnDark :: Html.Attribute r i
textIconOnDark =
    class "mdc-theme--text-icon-on-dark"



textPrimaryOnBackground :: Html.Attribute r i
textPrimaryOnBackground =
    class "mdc-theme--text-primary-on-background"



textSecondaryOnBackground :: Html.Attribute r i
textSecondaryOnBackground =
    class "mdc-theme--text-secondary-on-background"



textHintOnBackground :: Html.Attribute r i
textHintOnBackground =
    class "mdc-theme--text-hint-on-background"



textDisabledOnBackground :: Html.Attribute r i
textDisabledOnBackground =
    class "mdc-theme--text-disabled-on-background"



textIconOnBackground :: Html.Attribute r i
textIconOnBackground =
    class "mdc-theme--text-icon-on-background"
