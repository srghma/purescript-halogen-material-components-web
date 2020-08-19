module HalogenMWC.Snackbar where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Data.List (List(..), (:))
import Data.List as List

type Queue r i =
  { messages :: Array (Tuple MessageId (Message r i))
  , nextMessageId :: MessageId
  }

newtype MessageId = MessageId Int

inc :: MessageId -> MessageId
inc (MessageId messageId) = MessageId (messageId + 1)

initialQueue :: Queue r i
initialQueue =
  { messages: []
  , nextMessageId: MessageId 0
  }

close :: MessageId -> Queue r i -> Queue r i
close messageId queue =
  { messages:
      case queue.messages of
          [] -> []
          (Tuple currentMessageId _) : otherMessages ->
              if currentMessageId == messageId
                then otherMessages
                else queue.messages
  , nextMessageId: queue.nextMessageId
  }

addMessage :: Message r i -> Queue r i -> Queue r i
addMessage message_ queue =
  { messages: queue.messages <> List.singleton (Tuple queue.nextMessageId message_ )
  , nextMessageId: inc queue.nextMessageId
  }

type Config r i =
        { closeOnEscape :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClosed :: MessageId -> r i
        }

config :: { onClosed :: MessageId -> r i } -> Config r i
config { onClosed } =
        { closeOnEscape: False
        , additionalAttributes: []
        , onClosed: onClosed
        }

snackbar :: Config r i -> Queue r i -> Html r i
snackbar config_ queue =
    let
        (Tuple currentMessageId currentMessage) =
            Array.head queue.messages
                # map (Tuple.mapSecond Just)
                # Maybe.withDefault ( Tuple (MessageId -1) Nothing )
    in
    HH.element "mdc-snackbar"
        (Array.filterMap identity
            [ rootCs
            , closeOnEscapeProp config_
            , leadingCs currentMessage
            , stackedCs currentMessage
            , messageIdProp currentMessageId
            , timeoutMsProp currentMessage
            , closedHandler currentMessageId config_
            ]
            <> config_.additionalAttributes
        )
        [ surfaceElt currentMessageId (Maybe.withDefault (message "") currentMessage) ]

data Message r i
    = Message
        { label :: String
        , actionButton :: Maybe String
        , onActionButtonClick :: Maybe (MessageId -> r i)
        , actionIcon :: Maybe String
        , onActionIconClick :: Maybe (MessageId -> r i)
        , leading :: Boolean
        , stacked :: Boolean
        , timeoutMs :: Maybe Int
        }

{-| Default snackbar message (empty label)
-}
message :: String -> Message r i
message label =
    Message
        { label: label
        , actionButton: Nothing
        , onActionButtonClick: Nothing
        , actionIcon: Nothing
        , onActionIconClick: Nothing
        , leading: False
        , stacked: False
        , timeoutMs: Just 5000
        }

rootCs :: Maybe (IProp r i)
rootCs =
    Just (HP.class_ mdc_snackbar)

closeOnEscapeProp :: Config r i -> Maybe (IProp r i)
closeOnEscapeProp { closeOnEscape } =
    Just (HH.Attributes.property "closeOnEscape" (Encode.bool closeOnEscape))

leadingCs :: Maybe (Message r i) -> Maybe (IProp r i)
leadingCs message_ =
    Maybe.andThen
        (\(Message { leading }) ->
            if leading then
                Just (HP.class_ mdc_snackbar____leading)

            else
                Nothing
        )
        message_

stackedCs :: Maybe (Message r i) -> Maybe (IProp r i)
stackedCs message_ =
    Maybe.andThen
        (\(Message { stacked }) ->
            if stacked then
                Just (HP.class_ mdc_snackbar____stacked)

            else
                Nothing
        )
        message_

messageIdProp :: MessageId -> Maybe (IProp r i)
messageIdProp (MessageId messageId) =
    Just (HH.Attributes.property "messageId" (Encode.int messageId))

timeoutMsProp :: Maybe (Message r i) -> Maybe (IProp r i)
timeoutMsProp message_ =
    let
        normalizedTimeoutMs =
            message_
                # Maybe.andThen
                    (\(Message { timeoutMs }) -> map (clamp 4000 10000) timeoutMs)
                # Maybe.withDefault indefiniteTimeout

        indefiniteTimeout =
            -1
    in
    Just (HH.Attributes.property "timeoutMs" (Encode.int normalizedTimeoutMs))

closedHandler :: MessageId -> Config r i -> Maybe (IProp r i)
closedHandler messageId { onClosed } =
    Just (HH.Events.on "MDCSnackbar:closed" (Decode.succeed (onClosed messageId)))

ariaStatusRoleAttr :: IProp r i
ariaStatusRoleAttr =
    HH.Attributes.attribute "aria-role" "status"

ariaPoliteLiveAttr :: IProp r i
ariaPoliteLiveAttr =
    HH.Attributes.attribute "aria-live" "polite"

surfaceElt :: MessageId -> Message r i -> Html r i
surfaceElt messageId message_ =
    HH.div [ HP.class_ mdc_snackbar__surface ]
        [ labelElt message_
        , actionsElt messageId message_
        ]

labelElt :: Message r i -> Html r i
labelElt (Message { label }) =
    HH.div [ HP.class_ mdc_snackbar__label, ariaStatusRoleAttr, ariaPoliteLiveAttr ]
        [ text label ]

actionsElt :: MessageId -> Message r i -> Html r i
actionsElt messageId message_ =
    HH.div [ HP.class_ mdc_snackbar__actions ]
        (Array.filterMap identity
            [ actionButtonElt messageId message_
            , actionIconElt messageId message_
            ]
        )

actionButtonElt :: MessageId -> Message r i -> Maybe (Html r i)
actionButtonElt messageId (message_@(Message { actionButton })) =
    map
        (\actionButtonLabel ->
            HH.button
                (Array.filterMap identity
                    [ actionButtonCs
                    , actionButtonClickHandler messageId message_
                    ]
                )
                [ text actionButtonLabel ]
        )
        actionButton

actionButtonCs :: Maybe (IProp r i)
actionButtonCs =
    Just (HP.class_ "mdc-button mdc-snackbar__action")

actionButtonClickHandler :: MessageId -> Message r i -> Maybe (IProp r i)
actionButtonClickHandler messageId (Message { onActionButtonClick }) =
    map (HH.Events.onClick << (#) messageId) onActionButtonClick

actionIconElt :: MessageId -> Message r i -> Maybe (Html r i)
actionIconElt messageId (message_@(Message { actionIcon })) =
    map
        (\actionIconLabel ->
            HH.i
                (Array.filterMap identity
                    [ actionIconCs
                    , actionIconClickHandler messageId message_
                    ]
                )
                [ text actionIconLabel ]
        )
        actionIcon

actionIconCs :: Maybe (IProp r i)
actionIconCs =
    Just (HP.class_ "mdc-icon-button mdc-snackbar__dismiss material-icons")

actionIconClickHandler :: MessageId -> Message r i -> Maybe (IProp r i)
actionIconClickHandler messageId (Message { onActionIconClick }) =
    map (HH.Events.onClick << (#) messageId) onActionIconClick
