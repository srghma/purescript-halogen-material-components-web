module HalogenMWC.Icon (icon) where

{-| Icon renders a Material Icon.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Icon](#icon)


# Resources

  - [Material Icons](https://material.io/tools/icons/)


# Basic Usage

    import HalogenMWC.Icon as Icon

    main =
        Icon.icon [] "favorite"


# Icon

@docs icon

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



{-| Icon view function
-}
icon :: Array (IProp r i) -> String -> Html r i
icon additionalAttributes iconName =
    Html.i (class "material-icons" :: additionalAttributes) [ text iconName ]
