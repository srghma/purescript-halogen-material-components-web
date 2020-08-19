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
