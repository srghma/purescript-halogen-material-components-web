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

    import HalogenMWC.Button as Button
    import HalogenMWC.Dialog as Dialog

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

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA






{-| Configuration of a dialog
-}
type Config r i
    =
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }


{-| Default configuration of a dialog
-}
config :: Config r i
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }


{-| Specify whether a dialog is open
-}
setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user closes the dialog
-}
setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }


{-| Dialog content
-}
data Content r i =
    { title :: Maybe String
    , content :: Array (Html r i)
    , actions :: Array (Html r i)
    }


{-| Dialog view function
-}
dialog :: Config r i -> Content r i -> Html r i
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


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-dialog")


openProp :: Config r i -> Maybe (Html.Attribute r i)
openProp (Config { open }) =
    Just (Html.Attributes.property "open" (Encode.bool open))


roleAttr :: Maybe (Html.Attribute r i)
roleAttr =
    Just (Html.Attributes.attribute "role" "alertdialog")


ariaModalAttr :: Maybe (Html.Attribute r i)
ariaModalAttr =
    Just (Html.Attributes.attribute "aria-modal" "true")


closeHandler :: Config r i -> Maybe (Html.Attribute r i)
closeHandler (Config { onClose }) =
    Maybe.map (Html.Events.on "MDCDialog:close" << Decode.succeed) onClose


containerElt :: Content r i -> Html r i
containerElt content =
    Html.div [ class "mdc-dialog__container" ] [ surfaceElt content ]


surfaceElt :: Content r i -> Html r i
surfaceElt content =
    Html.div
        [ class "mdc-dialog__surface" ]
        (Array.filterMap identity
            [ titleElt content
            , contentElt content
            , actionsElt content
            ]
        )


titleElt :: Content r i -> Maybe (Html r i)
titleElt { title } =
    case title of
        Just title_ ->
            Just (Html.div [ class "mdc-dialog__title" ] [ text title_ ])

        Nothing ->
            Nothing


contentElt :: Content r i -> Maybe (Html r i)
contentElt { content } =
    Just (Html.div [ class "mdc-dialog__content" ] content)


actionsElt :: Content r i -> Maybe (Html r i)
actionsElt { actions } =
    if Array.isEmpty actions then
        Nothing

    else
        Just (Html.div [ class "mdc-dialog__actions" ] actions)


scrimElt :: Html r i
scrimElt =
    Html.div [ class "mdc-dialog__scrim" ] []
