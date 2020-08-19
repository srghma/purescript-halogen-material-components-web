module HalogenMWC.Snackbar where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

data Queue r i
    = Queue
        { messages :: Array ( MessageId, Message r i )
        , nextMessageId :: MessageId
        }

data MessageId
    = MessageId Int

inc :: MessageId -> MessageId
inc (MessageId messageId) =
    MessageId (messageId + 1)

initialQueue :: Queue r i
initialQueue =
    Queue
        { messages: []
        , nextMessageId: MessageId 0
        }

close :: MessageId -> Queue r i -> Queue r i
close messageId (Queue queue) =
    Queue $
        { queue
            { messages =
                case queue.messages of
                    [] ->
                        []

                    ( currentMessageId, _ ) :: otherMessages ->
                        if currentMessageId == messageId then
                            otherMessages

                        else
                            queue.messages
        }

addMessage :: Message r i -> Queue r i -> Queue r i
addMessage message_ (Queue queue) =
    Queue
        { queue
            { messages = queue.messages <> [ ( queue.nextMessageId, message_ ) ]
            , nextMessageId: inc queue.nextMessageId
        }

type Config r i
    =
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
snackbar (config_@{ additionalAttributes }) ((Queue { messages, nextMessageId }) as queue) =
    let
        ( currentMessageId, currentMessage ) =
            Array.head messages
                # map (Tuple.mapSecond Just)
                # Maybe.withDefault ( MessageId -1, Nothing )
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
            <> additionalAttributes
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

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_snackbar)

closeOnEscapeProp :: Config r i -> Maybe (HH.Attribute r i)
closeOnEscapeProp { closeOnEscape } =
    Just (HH.Attributes.property "closeOnEscape" (Encode.bool closeOnEscape))

leadingCs :: Maybe (Message r i) -> Maybe (HH.Attribute r i)
leadingCs message_ =
    Maybe.andThen
        (\(Message { leading }) ->
            if leading then
                Just (HP.class_ mdc_snackbar____leading)

            else
                Nothing
        )
        message_

stackedCs :: Maybe (Message r i) -> Maybe (HH.Attribute r i)
stackedCs message_ =
    Maybe.andThen
        (\(Message { stacked }) ->
            if stacked then
                Just (HP.class_ mdc_snackbar____stacked)

            else
                Nothing
        )
        message_

messageIdProp :: MessageId -> Maybe (HH.Attribute r i)
messageIdProp (MessageId messageId) =
    Just (HH.Attributes.property "messageId" (Encode.int messageId))

timeoutMsProp :: Maybe (Message r i) -> Maybe (HH.Attribute r i)
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

closedHandler :: MessageId -> Config r i -> Maybe (HH.Attribute r i)
closedHandler messageId { onClosed } =
    Just (HH.Events.on "MDCSnackbar:closed" (Decode.succeed (onClosed messageId)))

ariaStatusRoleAttr :: HH.Attribute r i
ariaStatusRoleAttr =
    HH.Attributes.attribute "aria-role" "status"

ariaPoliteLiveAttr :: HH.Attribute r i
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
actionButtonElt messageId ((Message { actionButton }) as message_) =
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

actionButtonCs :: Maybe (HH.Attribute r i)
actionButtonCs =
    Just (HP.class_ "mdc-button mdc-snackbar__action")

actionButtonClickHandler :: MessageId -> Message r i -> Maybe (HH.Attribute r i)
actionButtonClickHandler messageId (Message { onActionButtonClick }) =
    map (HH.Events.onClick << (#) messageId) onActionButtonClick

actionIconElt :: MessageId -> Message r i -> Maybe (Html r i)
actionIconElt messageId ((Message { actionIcon }) as message_) =
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

actionIconCs :: Maybe (HH.Attribute r i)
actionIconCs =
    Just (HP.class_ "mdc-icon-button mdc-snackbar__dismiss material-icons")

actionIconClickHandler :: MessageId -> Message r i -> Maybe (HH.Attribute r i)
actionIconClickHandler messageId (Message { onActionIconClick }) =
    map (HH.Events.onClick << (#) messageId) onActionIconClick
