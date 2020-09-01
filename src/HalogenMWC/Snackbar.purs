module HalogenMWC.Snackbar where

import Halogen (AttrName(..), ClassName, ElemName(..), PropName(..))
import Material.Classes.Button (mdc_button)
import Material.Classes.Snackbar (mdc_snackbar, mdc_snackbar____leading, mdc_snackbar____stacked, mdc_snackbar__action, mdc_snackbar__actions, mdc_snackbar__dismiss, mdc_snackbar__label, mdc_snackbar__surface)
import MaterialIconsFont.Classes (material_icons)
import Prelude
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Web.Event.Event (EventType(..))
import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.IconButton (mdc_icon_button)
import Control.Bind (bindFlipped)

type Queue i
  = { messages :: Array (Tuple MessageId (Message i))
    , nextMessageId :: MessageId
    }

newtype MessageId
  = MessageId Int

derive newtype instance eqMessageId :: Eq MessageId

inc :: MessageId -> MessageId
inc (MessageId messageId) = MessageId (messageId + 1)

initialQueue :: forall i. Queue i
initialQueue =
  { messages: []
  , nextMessageId: MessageId 0
  }

close :: forall i. MessageId -> Queue i -> Queue i
close messageId queue =
  { messages:
    case Array.uncons queue.messages of
      Nothing -> []
      Just { head: Tuple currentMessageId _, tail } ->
        if currentMessageId == messageId then
          tail
        else
          queue.messages
  , nextMessageId: queue.nextMessageId
  }

addMessage :: forall i. Message i -> Queue i -> Queue i
addMessage message queue =
  { messages: queue.messages <> [ Tuple queue.nextMessageId message ]
  , nextMessageId: inc queue.nextMessageId
  }

type Config i
  = { closeOnEscape :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , onClosed :: MessageId -> i
    }

defaultConfig :: forall i. (MessageId -> i) -> Config i
defaultConfig onClosed =
  { closeOnEscape: false
  , additionalAttributes: []
  , onClosed
  }

snackbar :: forall w i. Config i -> Queue i -> HH.HTML w i
snackbar config queue =
  let
    (Tuple (MessageId currentMessageId) currentMessage) = case Array.head queue.messages of
      Nothing -> Tuple (MessageId (-1)) Nothing
      Just (Tuple messageId message) -> Tuple messageId (Just message)
  in
    HH.element (ElemName "mdc-snackbar")
      ( [ HP.classes
            $ Array.catMaybes
                [ Just $ mdc_snackbar
                , leadingCs currentMessage
                , stackedCs currentMessage
                ]
        , HP.prop (PropName "closeOnEscape") config.closeOnEscape
        , HP.prop (PropName "messageId") currentMessageId
        , timeoutMsProp currentMessage
        , closedHandler (MessageId currentMessageId) config
        ]
          <> config.additionalAttributes
      )
      [ surfaceElt (MessageId currentMessageId) (Maybe.fromMaybe (defaultMessage "") currentMessage) ]

newtype Message i
  = Message
  { label :: String
  , actionButton :: Maybe String
  , onActionButtonClick :: Maybe (MessageId -> i)
  , actionIcon :: Maybe String
  , onActionIconClick :: Maybe (MessageId -> i)
  , leading :: Boolean
  , stacked :: Boolean
  , timeoutMs :: Maybe Int
  }

defaultMessage :: forall i. String -> Message i
defaultMessage label =
  Message
    { label
    , actionButton: Nothing
    , onActionButtonClick: Nothing
    , actionIcon: Nothing
    , onActionIconClick: Nothing
    , leading: false
    , stacked: false
    , timeoutMs: Just 5000
    }

leadingCs :: forall i. Maybe (Message i) -> Maybe ClassName
leadingCs =
  bindFlipped
    ( \(Message message) ->
        if message.leading then
          Just mdc_snackbar____leading
        else
          Nothing
    )

stackedCs :: forall i. Maybe (Message i) -> Maybe ClassName
stackedCs =
  bindFlipped
    ( \(Message message) ->
        if message.stacked then
          Just mdc_snackbar____stacked
        else
          Nothing
    )

timeoutMsProp :: forall r i. Maybe (Message i) -> IProp r i
timeoutMsProp message =
  let
    indefiniteTimeout = -1

    normalizedTimeoutMs =
      message
        # bindFlipped (\(Message message') -> map (clamp 4000 10000) message'.timeoutMs)
        # Maybe.fromMaybe indefiniteTimeout
  in
    HP.prop (PropName "timeoutMs") normalizedTimeoutMs

closedHandler :: forall r i. MessageId -> Config i -> IProp r i
closedHandler messageId config = HE.handler (EventType "MDCSnackbar:closed") (\_ -> config.onClosed messageId)

surfaceElt :: forall w i. MessageId -> Message i -> HH.HTML w i
surfaceElt messageId message =
  HH.div [ HP.class_ mdc_snackbar__surface ]
    [ labelElt message
    , actionsElt messageId message
    ]

labelElt :: forall w i. Message i -> HH.HTML w i
labelElt (Message message) =
  HH.div
    [ HP.class_ mdc_snackbar__label
    , HP.attr (AttrName "aria-role") "status"
    , HP.attr (AttrName "aria-live") "polite"
    ]
    [ HH.text message.label ]

actionsElt :: forall w i. MessageId -> Message i -> HH.HTML w i
actionsElt messageId message =
  HH.div [ HP.class_ mdc_snackbar__actions ]
    ( Array.catMaybes
        [ actionButtonElt messageId message
        , actionIconElt messageId message
        ]
    )

actionButtonElt :: forall w i. MessageId -> Message i -> Maybe (HH.HTML w i)
actionButtonElt messageId (Message message) =
  map
    ( \actionButtonLabel ->
        HH.button
          ( Array.catMaybes
              [ Just $ HP.classes [ mdc_button, mdc_snackbar__action ]
              , actionButtonClickHandler messageId (Message message)
              ]
          )
          [ HH.text actionButtonLabel ]
    )
    message.actionButton

actionButtonClickHandler :: forall i. MessageId -> Message i -> Maybe (IProp I.HTMLbutton i)
actionButtonClickHandler messageId (Message message) = map (\f -> HE.onClick (\_ -> f messageId)) message.onActionButtonClick

actionIconElt :: forall w i. MessageId -> Message i -> Maybe (HH.HTML w i)
actionIconElt messageId (Message message) =
  map
    ( \actionIconLabel ->
        HH.i
          ( Array.catMaybes
              [ Just $ HP.classes [ mdc_icon_button, mdc_snackbar__dismiss, material_icons ]
              , actionIconClickHandler messageId (Message message)
              ]
          )
          [ HH.text actionIconLabel ]
    )
    message.actionIcon

actionIconClickHandler :: forall i. MessageId -> Message i -> Maybe (IProp I.HTMLi i)
actionIconClickHandler messageId (Message message) = map (\f -> HE.onClick (\_ -> f messageId)) message.onActionIconClick
