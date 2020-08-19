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
    HP.class_ mdc_theme____primary



secondary :: Html.Attribute r i
secondary =
    HP.class_ mdc_theme____secondary



background :: Html.Attribute r i
background =
    HP.class_ mdc_theme____background



surface :: Html.Attribute r i
surface =
    HP.class_ mdc_theme____surface


{-| Sets the text color to the theme on-primary color

The theme's on-primary color is a text color that works best on a primary color
background.

-}
onPrimary :: Html.Attribute r i
onPrimary =
    HP.class_ "mdc-theme--on-primary"


{-| Sets the text color to the theme on-secondary color

The theme's on-secondary color is a text color that works best on a secondary
color background.

-}
onSecondary :: Html.Attribute r i
onSecondary =
    HP.class_ "mdc-theme--on-secondary"


{-| Sets the text color to the theme on-surface color

The theme's on-surface color is a text color that works best on a surface
color background.

-}
onSurface :: Html.Attribute r i
onSurface =
    HP.class_ "mdc-theme--on-surface"



primaryBg :: Html.Attribute r i
primaryBg =
    HP.class_ "mdc-theme--primary-bg"



secondaryBg :: Html.Attribute r i
secondaryBg =
    HP.class_ "mdc-theme--secondary-bg"



textPrimaryOnLight :: Html.Attribute r i
textPrimaryOnLight =
    HP.class_ "mdc-theme--text-primary-on-light"



textSecondaryOnLight :: Html.Attribute r i
textSecondaryOnLight =
    HP.class_ "mdc-theme--text-secondary-on-light"



textHintOnLight :: Html.Attribute r i
textHintOnLight =
    HP.class_ "mdc-theme--text-hint-on-light"



textDisabledOnLight :: Html.Attribute r i
textDisabledOnLight =
    HP.class_ "mdc-theme--text-disabled-on-light"



textIconOnLight :: Html.Attribute r i
textIconOnLight =
    HP.class_ "mdc-theme--text-icon-on-light"



textPrimaryOnDark :: Html.Attribute r i
textPrimaryOnDark =
    HP.class_ "mdc-theme--text-primary-on-dark"



textSecondaryOnDark :: Html.Attribute r i
textSecondaryOnDark =
    HP.class_ "mdc-theme--text-secondary-on-dark"



textHintOnDark :: Html.Attribute r i
textHintOnDark =
    HP.class_ "mdc-theme--text-hint-on-dark"



textDisabledOnDark :: Html.Attribute r i
textDisabledOnDark =
    HP.class_ "mdc-theme--text-disabled-on-dark"



textIconOnDark :: Html.Attribute r i
textIconOnDark =
    HP.class_ "mdc-theme--text-icon-on-dark"



textPrimaryOnBackground :: Html.Attribute r i
textPrimaryOnBackground =
    HP.class_ "mdc-theme--text-primary-on-background"



textSecondaryOnBackground :: Html.Attribute r i
textSecondaryOnBackground =
    HP.class_ "mdc-theme--text-secondary-on-background"



textHintOnBackground :: Html.Attribute r i
textHintOnBackground =
    HP.class_ "mdc-theme--text-hint-on-background"



textDisabledOnBackground :: Html.Attribute r i
textDisabledOnBackground =
    HP.class_ "mdc-theme--text-disabled-on-background"



textIconOnBackground :: Html.Attribute r i
textIconOnBackground =
    HP.class_ "mdc-theme--text-icon-on-background"
