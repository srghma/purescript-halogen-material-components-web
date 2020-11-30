module HalogenMWC.Utils where

import Protolude

import Data.Either (hush)
import Data.Lens (Lens')
import Data.Lens as Lens
import Effect.Timer as Timer
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event as Event
import Foreign (F, Foreign)
import Foreign (readBoolean, unsafeToForeign) as Foreign
import Foreign.Index (readProp) as Foreign
import Halogen (ClassName, PropName)
import Halogen.HTML (HTML) as HH
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event as E
import Web.Event.EventTarget as ET

fromPlainHTMLArray :: forall w i . Array (HH.HTML Void Void) -> Array (HH.HTML w i)
fromPlainHTMLArray = unsafeCoerce
-- | fromPlainHTMLArray = map fromPlainHTML

fromPlainIProp :: forall r i . IProp r Void -> IProp r i
fromPlainIProp = unsafeCoerce
-- | fromPlainIProp = map absurd

fromPlainIPropArray :: forall r i . Array (IProp r Void) -> Array (IProp r i)
fromPlainIPropArray = unsafeCoerce
-- | fromPlainIPropArray = map fromPlainIProp

prop :: forall value r i. PropName value -> PropValue -> IProp r i
prop = unsafeCoerce Property

propFromArrayInt :: Array Int -> PropValue
propFromArrayInt = unsafeCoerce

propFromArrayString :: Array String -> PropValue
propFromArrayString = unsafeCoerce

propFromArrayClassName :: Array ClassName -> PropValue
propFromArrayClassName = unsafeCoerce

-- NOTE: copied from Halogen.HTML.Events
addForeignPropHandler :: forall r i value. E.EventType -> String -> (Foreign -> F value) -> (value -> i) -> IProp r i
addForeignPropHandler key prop_ reader f = HE.handler' key $ E.currentTarget >=> \e -> either (const Nothing) (Just <<< f) $ runExcept $ go e
  where
  go a = reader <=< Foreign.readProp prop_ $ Foreign.unsafeToForeign a

-- TODO: MDCArray choses to send a change event to all checkboxes, thus we
-- have to check here if the state actually changed.
checkboxChangeHandler :: forall i r. (E.Event -> i) -> IProp r i
checkboxChangeHandler = \onChange -> HE.handler' (E.EventType "change") (handle onChange)
  where
  readChecked :: E.Event -> Maybe Boolean
  readChecked event =
    Foreign.unsafeToForeign event
      # (\f -> Foreign.readProp "target" f >>= Foreign.readProp "checked" >>= Foreign.readBoolean)
      # runExcept
      # hush

  handle onChange = \event -> case readChecked event of
    Nothing -> Nothing
    Just _ -> Just $ onChange event

foreign import checkBrowserSupportsPassive :: Effect Boolean

unsafePassiveIfSupportsAddEventListenerOptions :: AddEventListenerOptions
unsafePassiveIfSupportsAddEventListenerOptions =
  if unsafePerformEffect checkBrowserSupportsPassive
    then AddEventListenerOptionsNewBrowsers { capture: false, passive: true, once: false }
    else AddEventListenerOptionsOldBrowsers false

-- https://dom.spec.whatwg.org/#dictdef-addeventlisteneroptions

data AddEventListenerOptions
  = AddEventListenerOptionsNewBrowsers
    { capture :: Boolean
    , passive :: Boolean
    , once :: Boolean
    }
  | AddEventListenerOptionsOldBrowsers
    Boolean

data EventListenerOptions
  = EventListenerOptionsNewBrowsers
    { capture :: Boolean
    }
  | EventListenerOptionsOldBrowsers
    Boolean

addEventListenerOptionsToEventListenerOptions :: AddEventListenerOptions -> EventListenerOptions
addEventListenerOptionsToEventListenerOptions (AddEventListenerOptionsNewBrowsers options) = EventListenerOptionsNewBrowsers { capture: options.capture }
addEventListenerOptionsToEventListenerOptions (AddEventListenerOptionsOldBrowsers useCapture) = EventListenerOptionsOldBrowsers useCapture

data AddEventListenerOptionsInternal

addEventListenerOptionsToInternal :: AddEventListenerOptions -> AddEventListenerOptionsInternal
addEventListenerOptionsToInternal (AddEventListenerOptionsNewBrowsers options) = unsafeCoerce options
addEventListenerOptionsToInternal (AddEventListenerOptionsOldBrowsers useCapture) = unsafeCoerce useCapture

data EventListenerOptionsInternal

eventListenerOptionsToInternal :: EventListenerOptions -> EventListenerOptionsInternal
eventListenerOptionsToInternal (EventListenerOptionsNewBrowsers options) = unsafeCoerce options
eventListenerOptionsToInternal (EventListenerOptionsOldBrowsers useCapture) = unsafeCoerce useCapture

foreign import addEventListenerWithOptions
  :: E.EventType
  -> ET.EventListener
  -> AddEventListenerOptionsInternal
  -> ET.EventTarget
  -> Effect Unit

foreign import removeEventListenerWithOptions
  :: E.EventType
  -> ET.EventListener
  -> EventListenerOptionsInternal
  -> ET.EventTarget
  -> Effect Unit

eventListenerEventSourceWithOptions
  :: E.EventType
  -> AddEventListenerOptions
  -> ET.EventTarget
  -> Event.Event E.Event
eventListenerEventSourceWithOptions eventType options target =
  Event.makeEvent \push -> do
    listener <- ET.eventListener push
    addEventListenerWithOptions eventType listener (addEventListenerOptionsToInternal options) target
    let removeOptions = eventListenerOptionsToInternal $ addEventListenerOptionsToEventListenerOptions options
    pure $ removeEventListenerWithOptions eventType listener removeOptions target

mkTimeoutEvent :: forall action . action -> Int -> Event.Event action
mkTimeoutEvent action time =
  Event.makeEvent \push -> do
    timeoutId <- Timer.setTimeout time (push action)
    pure $ Timer.clearTimeout timeoutId

eventListenerEventSourceWithOptionsMany
  :: Array E.EventType
  -> AddEventListenerOptions
  -> ET.EventTarget
  -> Event.Event E.Event
eventListenerEventSourceWithOptionsMany eventTypes options target =
  Event.makeEvent \push -> do
    listener <- ET.eventListener push

    let addOptions = addEventListenerOptionsToInternal unsafePassiveIfSupportsAddEventListenerOptions

    for_ eventTypes \eventType -> addEventListenerWithOptions eventType listener addOptions target

    let removeOptions = eventListenerOptionsToInternal $ addEventListenerOptionsToEventListenerOptions unsafePassiveIfSupportsAddEventListenerOptions

    pure $
      for_ eventTypes \eventType -> removeEventListenerWithOptions eventType listener removeOptions target

styleVar :: String -> String -> String
styleVar x y = x <> ": " <> y <> ";"

setEfficiently :: forall state part . Eq part => Lens' state part -> part -> state -> state
setEfficiently = setEfficientlyCustomEq (==)

setEfficientlyCustomEq :: forall state part . (part -> part -> Boolean) -> Lens' state part -> part -> state -> state
setEfficientlyCustomEq eq lens new state =
  if eq (Lens.view lens state) new
    then state
    else Lens.set lens new state
