module HalogenMWC.Dialog
    ( Config, config



    , dialog, Content
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA







type Config r i
    =
        { open :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClose :: Maybe r i
        }



config :: Config r i
config =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }



setOpen :: Boolean -> Config r i -> Config r i
setOpen open (Config config_) =
    Config { config_ | open = open }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnClose :: r i -> Config r i -> Config r i
setOnClose onClose (Config config_) =
    Config { config_ | onClose = Just onClose }



data Content r i =
    { title :: Maybe String
    , content :: Array (Html r i)
    , actions :: Array (Html r i)
    }



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
    Just (HP.class_ mdc_dialog)


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
    Html.div [ HP.class_ mdc_dialog__container ] [ surfaceElt content ]


surfaceElt :: Content r i -> Html r i
surfaceElt content =
    Html.div
        [ HP.class_ mdc_dialog__surface ]
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
            Just (Html.div [ HP.class_ mdc_dialog__title ] [ text title_ ])

        Nothing ->
            Nothing


contentElt :: Content r i -> Maybe (Html r i)
contentElt { content } =
    Just (Html.div [ HP.class_ mdc_dialog__content ] content)


actionsElt :: Content r i -> Maybe (Html r i)
actionsElt { actions } =
    if Array.isEmpty actions then
        Nothing

    else
        Just (Html.div [ HP.class_ mdc_dialog__actions ] actions)


scrimElt :: Html r i
scrimElt =
    Html.div [ HP.class_ mdc_dialog__scrim ] []
