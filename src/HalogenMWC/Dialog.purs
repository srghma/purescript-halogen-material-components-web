module HalogenMWC.Dialog where

import Prelude
import DOM.HTML.Indexed as I
import Web.Event.Event (Event, EventType(..))
import Data.Array as Array
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Dialog (mdc_dialog, mdc_dialog__actions, mdc_dialog__container, mdc_dialog__content, mdc_dialog__scrim, mdc_dialog__surface, mdc_dialog__title)

type Config i
  = { open :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClose :: Maybe (Event -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { open: false
  , additionalAttributes: []
  , onClose: Nothing
  }

type Content w i
  = { title :: Maybe String
    , content :: Array (HH.HTML w i)
    , actions :: Array (HH.HTML w i)
    }

dialog :: forall w i. Config i -> Content w i -> HH.HTML w i
dialog config content =
  HH.element (ElemName "mdc-dialog")
    ( [ HP.class_ mdc_dialog
      , HP.prop (PropName "open") config.open
      , HP.attr (AttrName "role") "alertdialog"
      , HP.attr (AttrName "aria-modal") "true"
      ]
        <> Array.catMaybes
            [ closeHandler config
            ]
        <> config.additionalAttributes
    )
    [ containerElt content
    , scrimElt
    ]

closeHandler :: forall r i. Config i -> Maybe (IProp r i)
closeHandler config = map (HE.handler (EventType "MDCDialog:close")) config.onClose

containerElt :: forall w i. Content w i -> HH.HTML w i
containerElt content = HH.div [ HP.class_ mdc_dialog__container ] [ surfaceElt content ]

surfaceElt :: forall w i. Content w i -> HH.HTML w i
surfaceElt content =
  HH.div
    [ HP.class_ mdc_dialog__surface ]
    ( Array.catMaybes
        [ titleElt content
        , contentElt content
        , actionsElt content
        ]
    )

titleElt :: forall w i. Content w i -> Maybe (HH.HTML w i)
titleElt { title } = case title of
  Just title_ -> Just (HH.div [ HP.class_ mdc_dialog__title ] [ HH.text title_ ])
  Nothing -> Nothing

contentElt :: forall w i. Content w i -> Maybe (HH.HTML w i)
contentElt { content } = Just (HH.div [ HP.class_ mdc_dialog__content ] content)

actionsElt :: forall w i. Content w i -> Maybe (HH.HTML w i)
actionsElt { actions } =
  if Array.null actions then
    Nothing
  else
    Just (HH.div [ HP.class_ mdc_dialog__actions ] actions)

scrimElt :: forall w i. HH.HTML w i
scrimElt = HH.div [ HP.class_ mdc_dialog__scrim ] []
