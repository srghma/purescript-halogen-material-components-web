module HalogenMWC.Snackbar
    ( Config, config


    , snackbar
    , Queue, initialQueue, MessageId, close
    , addMessage
    , message, Message







    ) where

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
        { messages = []
        , nextMessageId = MessageId 0
        }



close :: MessageId -> Queue r i -> Queue r i
close messageId (Queue queue) =
    Queue $
        { queue
            | messages =
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
            | messages = queue.messages ++ [ ( queue.nextMessageId, message_ ) ]
            , nextMessageId = inc queue.nextMessageId
        }



type Config r i
    =
        { closeOnEscape :: Boolean
        , additionalAttributes :: Array (IProp r i)
        , onClosed :: MessageId -> r i
        }



config :: { onClosed :: MessageId -> r i } -> Config r i
config { onClosed } =
    Config
        { closeOnEscape = False
        , additionalAttributes = []
        , onClosed = onClosed
        }



setCloseOnEscape :: Boolean -> Config r i -> Config r i
setCloseOnEscape closeOnEscape (Config config_) =
    Config { config_ | closeOnEscape = closeOnEscape }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



snackbar :: Config r i -> Queue r i -> Html r i
snackbar ((Config { additionalAttributes }) as config_) ((Queue { messages, nextMessageId }) as queue) =
    let
        ( currentMessageId, currentMessage ) =
            Array.head messages
                # Maybe.map (Tuple.mapSecond Just)
                # Maybe.withDefault ( MessageId -1, Nothing )
    in
    HH.node "mdc-snackbar"
        (Array.filterMap identity
            [ rootCs
            , closeOnEscapeProp config_
            , leadingCs currentMessage
            , stackedCs currentMessage
            , messageIdProp currentMessageId
            , timeoutMsProp currentMessage
            , closedHandler currentMessageId config_
            ]
            ++ additionalAttributes
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



setActionButton :: Maybe String -> Message r i -> Message r i
setActionButton actionButton (Message message_) =
    Message { message_ | actionButton = actionButton }



setOnActionButtonClick :: (MessageId -> r i) -> Message r i -> Message r i
setOnActionButtonClick onActionButtonClick (Message message_) =
    Message { message_ | onActionButtonClick = Just onActionButtonClick }



setActionIcon :: Maybe String -> Message r i -> Message r i
setActionIcon actionIcon (Message message_) =
    Message { message_ | actionIcon = actionIcon }



setOnActionIconClick :: (MessageId -> r i) -> Message r i -> Message r i
setOnActionIconClick onActionIconClick (Message message_) =
    Message { message_ | onActionIconClick = Just onActionIconClick }



setLeading :: Boolean -> Message r i -> Message r i
setLeading leading (Message message_) =
    Message { message_ | leading = leading }



setStacked :: Boolean -> Message r i -> Message r i
setStacked stacked (Message message_) =
    Message { message_ | stacked = stacked }



setTimeoutMs :: Maybe Int -> Message r i -> Message r i
setTimeoutMs timeoutMs (Message message_) =
    Message { message_ | timeoutMs = timeoutMs }


{-| Default snackbar message (empty label)
-}
message :: String -> Message r i
message label =
    Message
        { label = label
        , actionButton = Nothing
        , onActionButtonClick = Nothing
        , actionIcon = Nothing
        , onActionIconClick = Nothing
        , leading = False
        , stacked = False
        , timeoutMs = Just 5000
        }


rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_snackbar)


closeOnEscapeProp :: Config r i -> Maybe (HH.Attribute r i)
closeOnEscapeProp (Config { closeOnEscape }) =
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
                    (\(Message { timeoutMs }) -> Maybe.map (clamp 4000 10000) timeoutMs)
                # Maybe.withDefault indefiniteTimeout

        indefiniteTimeout =
            -1
    in
    Just (HH.Attributes.property "timeoutMs" (Encode.int normalizedTimeoutMs))


closedHandler :: MessageId -> Config r i -> Maybe (HH.Attribute r i)
closedHandler messageId (Config { onClosed }) =
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
    Maybe.map
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
    Maybe.map (HH.Events.onClick << (#) messageId) onActionButtonClick


actionIconElt :: MessageId -> Message r i -> Maybe (Html r i)
actionIconElt messageId ((Message { actionIcon }) as message_) =
    Maybe.map
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
    Maybe.map (HH.Events.onClick << (#) messageId) onActionIconClick
