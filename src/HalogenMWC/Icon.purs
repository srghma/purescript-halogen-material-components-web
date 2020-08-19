module HalogenMWC.Icon (icon)

{-| Icon renders a Material Icon.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Icon](#icon)


# Resources

  - [Material Icons](https://material.io/tools/icons/)


# Basic Usage

    import Material.Icon as Icon

    main =
        Icon.icon [] "favorite"


# Icon

@docs icon

-}

import Html (Html, text)



{-| Icon view function
-}
icon :: Array (IProp r i) -> String -> Html r i
icon additionalAttributes iconName =
    Html.i (class "material-icons" :: additionalAttributes) [ text iconName ]
