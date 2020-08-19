module HalogenMWC.Dialog
    ( Config, config
    , setOnClose
    , setOpen
    , setAttributes
    , dialog, Content
    ) where

{-| Dialogs inform users about a task and can contain critical information,
require decisions, or involve multiple tasks.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Dialog](#dialog)


# Resources

  - [Demo: Dialogs](https://aforemny.github.io/material-components-web-elm/#dialog)
  - [Material Design Guidelines: Dialogs](https://material.io/go/design-dialogs)
  - [MDC Web: Dialog](https://github.com/material-components/material-components-web/tree/master/packages/mdc-dialog)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-dialog#sass-mixins)


# Basic Usage

    import Material.Button as Button
    import Material.Dialog as Dialog

    data Msg
        = Closed

    main =
        Dialog.dialog
            (Dialog.config
                |> Dialog.setOpen True
                |> Dialog.setOnClose Closed
            )
            { title = Nothing
            , content = [ text "Discard draft?" ]
            , actions =
                [ Button.text
                    (Button.config |> Button.setOnClick Closed)
                    "Cancel"
                , Button.text
                    (Button.config |> Button.setOnClick Closed)
                    "Discard"
                ]
            }


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClose
@docs setOpen
@docs setAttributes


# Dialog

@docs dialog, Content

-}

import Html (Html, text)
import Html.Attributes (class)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode


{-| Configuration of a dialog
-}
type Config r i
    = Config
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe msg
        }


{-| Default configuration of a dialog
-}
config :: Config msg
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }


{-| Specify whether a dialog is open
-}
setOpen :: Boolean -> Config msg -> Config msg
setOpen open (Config config_) =
    Config { config_ | open = open }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user closes the dialog
-}
setOnClose :: msg -> Config msg -> Config msg
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }


{-| Dialog content
-}
data Content msg =
    { title :: Maybe String
    , content :: Array (Html msg)
    , actions :: Array (Html msg)
    }


{-| Dialog view function
-}
dialog :: Config msg -> Content msg -> Html msg
dialog ((Config { additionalAttributes }) as config_) content =
    Html.node "mdc-dialog"
        (Array.filterMap identity
            [ rootCs
            , openProp config_
            , roleAttr
            , ariaModalAttr
            , closeHandler config_
            ]
            ++ additionalAttributes
        )
        [ containerElt content
        , scrimElt
        ]


rootCs :: Maybe (Html.Attribute msg)
rootCs =
    Just (class "mdc-dialog")


openProp :: Config msg -> Maybe (Html.Attribute msg)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


roleAttr :: Maybe (Html.Attribute msg)
roleAttr =
    Just (Html.Attributes.attribute "role" "alertdialog")


ariaModalAttr :: Maybe (Html.Attribute msg)
ariaModalAttr =
    Just (Html.Attributes.attribute "aria-modal" "true")


closeHandler :: Config msg -> Maybe (Html.Attribute msg)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDialog:close" << Decode.succeed) onClose


containerElt :: Content msg -> Html msg
containerElt content =
    Html.div [ class "mdc-dialog__container" ] [ surfaceElt content ]


surfaceElt :: Content msg -> Html msg
surfaceElt content =
    Html.div
        [ class "mdc-dialog__surface" ]
        (Array.filterMap identity
            [ titleElt content
            , contentElt content
            , actionsElt content
            ]
        )


titleElt :: Content msg -> Maybe (Html msg)
titleElt { title } =
    case title of
        Just title_ ->
            Just (Html.div [ class "mdc-dialog__title" ] [ text title_ ])

        Nothing ->
            Nothing


contentElt :: Content msg -> Maybe (Html msg)
contentElt { content } =
    Just (Html.div [ class "mdc-dialog__content" ] content)


actionsElt :: Content msg -> Maybe (Html msg)
actionsElt { actions } =
    if Array.isEmpty actions then
        Nothing

    else
        Just (Html.div [ class "mdc-dialog__actions" ] actions)


scrimElt :: Html msg
scrimElt =
    Html.div [ class "mdc-dialog__scrim" ] []
