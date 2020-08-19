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



type Config r i
    =
        { outlined :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { outlined = False
        , additionalAttributes = []
        }



setOutlined :: Boolean -> Config r i -> Config r i
setOutlined outlined (Config config_) =
    Config { config_ | outlined = outlined }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



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
                    [ Just (HP.class_ "mdc-card__actions")
                    , if fullBleed then
                        Just (HP.class_ "mdc-card__actions--full-bleed")

                      else
                        Nothing
                    ]
                )
                (Array.concat
                    [ if not (Array.isEmpty buttons) then
                        [ Html.div [ HP.class_ "mdc-card__action-buttons" ]
                            (Array.map (\(Button button_) -> button_) buttons)
                        ]

                      else
                        []
                    , if not (Array.isEmpty icons) then
                        [ Html.div [ HP.class_ "mdc-card__action-icons" ]
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
    Just (HP.class_ "mdc-card")


outlinedCs :: Config r i -> Maybe (Html.Attribute r i)
outlinedCs (Config { outlined }) =
    if outlined then
        Just (HP.class_ "mdc-card--outlined")

    else
        Nothing



data Content r i =
    { blocks :: Array (Block r i)
    , actions :: Maybe (Actions r i)
    }



data Block r i
    = Block (Html r i)



block :: Html r i -> Block r i
block =
    Block


data Aspect
    = Square
    | SixteenToNine


mediaView :: Maybe Aspect -> Array (IProp r i) -> String -> Block r i
mediaView aspect additionalAttributes backgroundImage =
    Block $
        Html.div
            (Array.filterMap identity
                [ mediaCs
                , backgroundImageAttr backgroundImage
                , aspectCs aspect
                ]
                ++ additionalAttributes
            )
            []



squareMedia :: Array (IProp r i) -> String -> Block r i
squareMedia additionalAttributes backgroundImage =
    mediaView (Just Square) additionalAttributes backgroundImage



sixteenToNineMedia :: Array (IProp r i) -> String -> Block r i
sixteenToNineMedia additionalAttributes backgroundImage =
    mediaView (Just SixteenToNine) additionalAttributes backgroundImage



media :: Array (IProp r i) -> String -> Block r i
media additionalAttributes backgroundImage =
    mediaView Nothing additionalAttributes backgroundImage


mediaCs :: Maybe (Html.Attribute r i)
mediaCs =
    Just (HP.class_ "mdc-card__media")


backgroundImageAttr :: String -> Maybe (Html.Attribute r i)
backgroundImageAttr url =
    Just (style "background-image" ("url(\"" ++ url ++ "\")"))


aspectCs :: Maybe Aspect -> Maybe (Html.Attribute r i)
aspectCs aspect =
    case aspect of
        Just Square ->
            Just (HP.class_ "mdc-card__media--square")

        Just SixteenToNine ->
            Just (HP.class_ "mdc-card__media--16-9")

        Nothing ->
            Nothing



primaryAction :: Array (IProp r i) -> Array (Block r i) -> Array (Block r i)
primaryAction additionalAttributes blocks =
    [ Block $
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
    HP.class_ "mdc-card__primary-action"


tabIndexProp :: Int -> Html.Attribute r i
tabIndexProp tabIndex =
    Html.Attributes.property "tabIndex" (Encode.int tabIndex)



data Actions r i
    = Actions
        { buttons :: Array (Button r i)
        , icons :: Array (Icon r i)
        , fullBleed :: Boolean
        }



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



data Button r i
    = Button (Html r i)



button :: Button.Config r i -> String -> Button r i
button (Material.Button.Internal.Config buttonConfig) label =
    Button $
        Button.text
            (Material.Button.Internal.Config
                { buttonConfig
                    | additionalAttributes =
                        HP.class_ "mdc-card__action"
                            :: HP.class_ "mdc-card__action--button"
                            :: buttonConfig.additionalAttributes
                }
            )
            label



data Icon r i
    = Icon (Html r i)



icon :: IconButton.Config r i -> String -> Icon r i
icon (Material.IconButton.Internal.Config iconButtonConfig) iconName =
    Icon $
        IconButton.iconButton
            (Material.IconButton.Internal.Config
                { iconButtonConfig
                    | additionalAttributes =
                        HP.class_ "mdc-card__action"
                            :: HP.class_ "mdc-card__action--icon"
                            :: iconButtonConfig.additionalAttributes
                }
            )
            iconName
