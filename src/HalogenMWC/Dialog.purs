module HalogenMWC.Dialog where

import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type Config r i
  = { open :: Boolean
    , additionalAttributes :: Array (IProp r i)
    , onClose :: Maybe r i
    }

defaultConfig :: Config r i
defaultConfig =
  { open: false
  , additionalAttributes: []
  , onClose: Nothing
  }

type Content r i
  = { title :: Maybe String
    , content :: Array (HH.HTML w i)
    , actions :: Array (HH.HTML w i)
    }

dialog :: Config r i -> Content r i -> HH.HTML w i
dialog (config_@{ additionalAttributes }) content =
  HH.element "mdc-dialog"
    ( Array.catMaybes
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

rootCs :: Maybe (IProp r i)
rootCs = Just (HP.class_ mdc_dialog)

openProp :: Config r i -> Maybe (IProp r i)
openProp { open } = Just (HP.prop "open" open)

roleAttr :: Maybe (IProp r i)
roleAttr = Just (HP.attr "role" "alertdialog")

ariaModalAttr :: Maybe (IProp r i)
ariaModalAttr = Just (HP.attr "aria-modal" "true")

closeHandler :: Config r i -> Maybe (IProp r i)
closeHandler { onClose } = map (HH.Events.on "MDCDialog:close" << Decode.succeed) onClose

containerElt :: Content r i -> HH.HTML w i
containerElt content = HH.div [ HP.class_ mdc_dialog__container ] [ surfaceElt content ]

surfaceElt :: Content r i -> HH.HTML w i
surfaceElt content =
  HH.div
    [ HP.class_ mdc_dialog__surface ]
    ( Array.catMaybes
        [ titleElt content
        , contentElt content
        , actionsElt content
        ]
    )

titleElt :: Content r i -> Maybe (HH.HTML w i)
titleElt { title } = case title of
  Just title_ -> Just (HH.div [ HP.class_ mdc_dialog__title ] [ text title_ ])
  Nothing -> Nothing

contentElt :: Content r i -> Maybe (HH.HTML w i)
contentElt { content } = Just (HH.div [ HP.class_ mdc_dialog__content ] content)

actionsElt :: Content r i -> Maybe (HH.HTML w i)
actionsElt { actions } =
  if Array.isEmpty actions then
    Nothing
  else
    Just (HH.div [ HP.class_ mdc_dialog__actions ] actions)

scrimElt :: HH.HTML w i
scrimElt = HH.div [ HP.class_ mdc_dialog__scrim ] []
