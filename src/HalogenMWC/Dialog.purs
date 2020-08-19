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

defaultConfig :: Config r i
defaultConfig =
    Config
        { open = False
        , additionalAttributes = []
        , onClose = Nothing
        }

type Content r i =
    { title :: Maybe String
    , content :: Array (Html r i)
    , actions :: Array (Html r i)
    }

dialog :: Config r i -> Content r i -> Html r i
dialog (config_@{ additionalAttributes }) content =
    HH.node "mdc-dialog"
        (Array.filterMap identity
            [ rootCs
            , openProp config_
            , roleAttr
            , ariaModalAttr
            , closeHandler config_
            ]
            <> additionalAttributes
        )
        [ containerElt content
        , scrimElt
        ]

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_dialog)

openProp :: Config r i -> Maybe (HH.Attribute r i)
openProp { open } =
    Just (HH.Attributes.property "open" (Encode.bool open))

roleAttr :: Maybe (HH.Attribute r i)
roleAttr =
    Just (HH.Attributes.attribute "role" "alertdialog")

ariaModalAttr :: Maybe (HH.Attribute r i)
ariaModalAttr =
    Just (HH.Attributes.attribute "aria-modal" "true")

closeHandler :: Config r i -> Maybe (HH.Attribute r i)
closeHandler { onClose } =
    map (HH.Events.on "MDCDialog:close" << Decode.succeed) onClose

containerElt :: Content r i -> Html r i
containerElt content =
    HH.div [ HP.class_ mdc_dialog__container ] [ surfaceElt content ]

surfaceElt :: Content r i -> Html r i
surfaceElt content =
    HH.div
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
            Just (HH.div [ HP.class_ mdc_dialog__title ] [ text title_ ])

        Nothing ->
            Nothing

contentElt :: Content r i -> Maybe (Html r i)
contentElt { content } =
    Just (HH.div [ HP.class_ mdc_dialog__content ] content)

actionsElt :: Content r i -> Maybe (Html r i)
actionsElt { actions } =
    if Array.isEmpty actions then
        Nothing

    else
        Just (HH.div [ HP.class_ mdc_dialog__actions ] actions)

scrimElt :: Html r i
scrimElt =
    HH.div [ HP.class_ mdc_dialog__scrim ] []
