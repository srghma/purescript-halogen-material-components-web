module HalogenMWC.Card
    ( Config, config
    
    
    , card, Content
    , Block
    , block
    , squareMedia, sixteenToNineMedia, media
    , primaryAction
    , Actions, actions
    , Button, button
    , Icon, icon
    , fullBleedActions
    ) where

{-| Cards contain content and actions about a single subject.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Card](#card)
      - [Outlined card](#outlined-card)
  - [Card Blocks](#card-blocks)
      - [Generic Block](#generic-block)
      - [Media Block](#media-block)
      - [Primary Action Block](#primary-action-block)
  - [Card Actions](#card-actions)
      - [Full Bleed Actions](#full-bleed-actions)
  - [Focus a Card](#focus-a-card)


# Resources

  - [Demo: Cards](https://aforemny.github.io/material-components-web-elm/#cards)
  - [Material Design Guidelines: Cards](https://material.io/go/design-cards)
  - [MDC Web: Card](https://github.com/material-components/material-components-web/tree/master/packages/mdc-card)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-card#sass-mixins)


# Basic Usage

    import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
    import HalogenMWC.Button as Button
    import HalogenMWC.Card as Card
    import HalogenMWC.IconButton as IconButton

    main =
        Card.card Card.config
            { blocks =
                [ Card.block <|
                    Html.div []
                        [ Html.h2 [] [ text "Title" ]
                        , Html.h3 [] [ text "Subtitle" ]
                        ]
                , Card.block <|
                    Html.div []
                        [ Html.p [] [ text "Lorem ipsum…" ] ]
                ]
            , actions =
                Just <|
                    Card.actions
                        { buttons =
                            [ Card.button Button.config "Visit" ]
                        , icons =
                            [ Card.icon IconButton.config
                                "favorite"
                            ]
                        }
            }


# Configuration

@docs Config, config


## Configuration Options

@docs setOutlined
@docs setAttributes


# Card

@docs card, Content


## Outlined Card

A card may display a border by setting its `setOutlined` configuration option
to `True`.

    Card.card
        (Card.config |> Card.setOutlined True)
        { blocks =
            [ Card.block <|
                Html.div [] [ Html.h1 [] [ text "Card" ] ]
            ]
        , actions = Nothing
        }


# Card Blocks

A card's primary content is comprised of _blocks_. Blocks may be comprised of
arbitrary HTML or a media element. Optionally, a group of card blocks can be
marked as the card's primary action which makes that group of blocks
interactable.

@docs Block


## Generic Block

Generic card blocks are the most common and allow you to specify card content
using arbitrary HTML. Note that you will have to carefully adjust styling such
as padding and typography yourself.

    Card.block <|
        Html.div []
            [ Html.h2 [] [ text "Title" ]
            , Html.h3 [] [ text "Subtitle" ]
            ]

@docs block


## Media Block

Cards may contain a media block usually as the first content block. The media
will be displayed using a background image, and you may chose from square or a
16 to 9 aspect ratio.

@docs squareMedia, sixteenToNineMedia, media


## Primary Action Block

A group of card blocks can be marked as the primary action of the card. A
primary action block may be clicked upon and displays a visual interaction
effect.

    Card.primaryAction
        [ Html.Events.onClick CardClicked ]
        [ Card.block <|
            Html.h2 [] [ text "Title" ]
        , Card.block <|
            Html.p [] [ text "Lorem ipsum…" ]
        ]

@docs primaryAction


# Card Actions

Card actions are comprised of buttons and icons. These are exposed as variants
to the standard buttons and icons, but they do share the same configuration.

    Card.actions
        { buttons =
            [ Card.button Button.config "View" ]
        , icons =
            [ Card.icon IconButton.config "favorite" ]
        }

@docs Actions, actions
@docs Button, button
@docs Icon, icon


## Card Full Bleed Actions

While a card's action buttons are usually left-aligned, a special case exists
when there is only a single button as card action.

@docs fullBleedActions


# Focus a Card

You may programatically focus a card by assigning an id attribute to it and use
`Browser.Dom.focus`.

Note that cards must have a primary action element to be focusable.

    Card.card
        (Card.config
            |> Card.setAttributes
                [ Html.Attributes.id "my-card" ]
        )
        { blocks = Card.primaryAction [] []
        , actions = Nothing
        }

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


import HalogenMWC.Button as Button
import HalogenMWC.Button.Internal
import HalogenMWC.IconButton as IconButton
import HalogenMWC.IconButton.Internal


{-| Configuration of a card
-}
type Config r i
    =
        { outlined :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a card
-}
config :: Config r i
config =
    Config
        { outlined = False
        , additionalAttributes = []
        }


{-| Specify whether a card should have a visual outline
-}
setOutlined :: Boolean -> Config r i -> Config r i
setOutlined outlined (Config config_) =
    Config { config_ | outlined = outlined }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Card view function
-}
card :: Config r i -> Content r i -> Html r i
card ((Config { additionalAttributes }) as config_) content =
    Html.node "mdc-card"
        (Array.filterMap identity
            [ rootCs
            , outlinedCs config_
            ]
            ++ additionalAttributes
        )
        (Array.concat
            [ blocksElt content
            , actionsElt content
            ]
        )


blocksElt :: Content r i -> Array (Html r i)
blocksElt { blocks } =
    Array.map (\(Block html) -> html) blocks


actionsElt :: Content r i -> Array (Html r i)
actionsElt content =
    case content.actions of
        Just (Actions { buttons, icons, fullBleed }) ->
            [ Html.div
                (Array.filterMap identity
                    [ Just (class "mdc-card__actions")
                    , if fullBleed then
                        Just (class "mdc-card__actions--full-bleed")

                      else
                        Nothing
                    ]
                )
                (Array.concat
                    [ if not (Array.isEmpty buttons) then
                        [ Html.div [ class "mdc-card__action-buttons" ]
                            (Array.map (\(Button button_) -> button_) buttons)
                        ]

                      else
                        []
                    , if not (Array.isEmpty icons) then
                        [ Html.div [ class "mdc-card__action-icons" ]
                            (Array.map (\(Icon icon_) -> icon_) icons)
                        ]

                      else
                        []
                    ]
                )
            ]

        Nothing ->
            []


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-card")


outlinedCs :: Config r i -> Maybe (Html.Attribute r i)
outlinedCs (Config { outlined }) =
    if outlined then
        Just (class "mdc-card--outlined")

    else
        Nothing


{-| The content of a card is comprised of _blocks_ and _actions_.
-}
data Content r i =
    { blocks :: Array (Block r i)
    , actions :: Maybe (Actions r i)
    }


{-| A card's content block
-}
data Block r i
    = Block (Html r i)


{-| Card block containing arbitrary HTML

    Card.block <|
        Html.div [] [ text "Lorem ipsum…" ]

-}
block :: Html r i -> Block r i
block =
    Block


data Aspect
    = Square
    | SixteenToNine


mediaView :: Maybe Aspect -> Array (IProp r i) -> String -> Block r i
mediaView aspect additionalAttributes backgroundImage =
    Block <|
        Html.div
            (Array.filterMap identity
                [ mediaCs
                , backgroundImageAttr backgroundImage
                , aspectCs aspect
                ]
                ++ additionalAttributes
            )
            []


{-| Card media block with a square aspect ratio
-}
squareMedia :: Array (IProp r i) -> String -> Block r i
squareMedia additionalAttributes backgroundImage =
    mediaView (Just Square) additionalAttributes backgroundImage


{-| Card media block with a 16:9 aspect ratio
-}
sixteenToNineMedia :: Array (IProp r i) -> String -> Block r i
sixteenToNineMedia additionalAttributes backgroundImage =
    mediaView (Just SixteenToNine) additionalAttributes backgroundImage


{-| Card media block of unspecified aspect ratio
-}
media :: Array (IProp r i) -> String -> Block r i
media additionalAttributes backgroundImage =
    mediaView Nothing additionalAttributes backgroundImage


mediaCs :: Maybe (Html.Attribute r i)
mediaCs =
    Just (class "mdc-card__media")


backgroundImageAttr :: String -> Maybe (Html.Attribute r i)
backgroundImageAttr url =
    Just (style "background-image" ("url(\"" ++ url ++ "\")"))


aspectCs :: Maybe Aspect -> Maybe (Html.Attribute r i)
aspectCs aspect =
    case aspect of
        Just Square ->
            Just (class "mdc-card__media--square")

        Just SixteenToNine ->
            Just (class "mdc-card__media--16-9")

        Nothing ->
            Nothing


{-| A card's primary action block
-}
primaryAction :: Array (IProp r i) -> Array (Block r i) -> Array (Block r i)
primaryAction additionalAttributes blocks =
    [ Block <|
        Html.div
            ([ primaryActionCs
             , tabIndexProp 0
             ]
                ++ additionalAttributes
            )
            (Array.map (\(Block html) -> html) blocks)
    ]


primaryActionCs :: Html.Attribute r i
primaryActionCs =
    class "mdc-card__primary-action"


tabIndexProp :: Int -> Html.Attribute r i
tabIndexProp tabIndex =
    Html.Attributes.property "tabIndex" (Encode.int tabIndex)


{-| Card actions type
-}
data Actions r i
    = Actions
        { buttons :: Array (Button r i)
        , icons :: Array (Icon r i)
        , fullBleed :: Boolean
        }


{-| Card actions

A card may contain as actions buttons as well as icons.

-}
actions :: { buttons :: Array (Button r i), icons :: Array (Icon r i) } -> Actions r i
actions { buttons, icons } =
    Actions { buttons = buttons, icons = icons, fullBleed = False }


{-| Card full bleed action

If a card's action is comprised of a single button, that button can be made
full width by using `cardFullBleedActions`.

    Card.fullBleedActions
        (Card.button Button.config "Visit")

-}
fullBleedActions :: Button r i -> Actions r i
fullBleedActions button_ =
    Actions { buttons = [ button_ ], icons = [], fullBleed = True }


{-| Card action's button type
-}
data Button r i
    = Button (Html r i)


{-| A card action button

    Card.button Button.config "Visit"

-}
button :: Button.Config r i -> String -> Button r i
button (Material.Button.Internal.Config buttonConfig) label =
    Button <|
        Button.text
            (Material.Button.Internal.Config
                { buttonConfig
                    | additionalAttributes =
                        class "mdc-card__action"
                            :: class "mdc-card__action--button"
                            :: buttonConfig.additionalAttributes
                }
            )
            label


{-| Card action's icon type
-}
data Icon r i
    = Icon (Html r i)


{-| Card action icon

    Card.icon IconButton.config "favorite"

-}
icon :: IconButton.Config r i -> String -> Icon r i
icon (Material.IconButton.Internal.Config iconButtonConfig) iconName =
    Icon <|
        IconButton.iconButton
            (Material.IconButton.Internal.Config
                { iconButtonConfig
                    | additionalAttributes =
                        class "mdc-card__action"
                            :: class "mdc-card__action--icon"
                            :: iconButtonConfig.additionalAttributes
                }
            )
            iconName
