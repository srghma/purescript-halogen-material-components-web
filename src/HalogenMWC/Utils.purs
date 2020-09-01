module HalogenMWC.Utils where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Either (hush) as Either
import Foreign (F, Foreign)
import Foreign (readBoolean, unsafeToForeign) as Foreign
import Foreign.Index (readProp) as Foreign
import Halogen (ClassName, PropName)
import Halogen.HTML.Core (Prop(..), PropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (IProp)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as EE

prop :: forall value r i. PropName value -> PropValue -> IProp r i
prop = unsafeCoerce Property

propFromArrayInt :: Array Int -> PropValue
propFromArrayInt = unsafeCoerce

propFromArrayString :: Array String -> PropValue
propFromArrayString = unsafeCoerce

propFromArrayClassName :: Array ClassName -> PropValue
propFromArrayClassName = unsafeCoerce

-- NOTE: copied from Halogen.HTML.Events
addForeignPropHandler :: forall r i value. EventType -> String -> (Foreign -> F value) -> (value -> i) -> IProp r i
addForeignPropHandler key prop_ reader f = HE.handler' key $ EE.currentTarget >=> \e -> either (const Nothing) (Just <<< f) $ runExcept $ go e
  where
  go a = reader <=< Foreign.readProp prop_ $ Foreign.unsafeToForeign a

-- TODO: MDCArray choses to send a change event to all checkboxes, thus we
-- have to check here if the state actually changed.
checkboxChangeHandler :: forall i r. (Event -> i) -> IProp r i
checkboxChangeHandler = \onChange -> HE.handler' (EventType "change") (handle onChange)
  where
  readChecked :: Event -> Maybe Boolean
  readChecked event =
    Foreign.unsafeToForeign event
      # (\f -> Foreign.readProp "target" f >>= Foreign.readProp "checked" >>= Foreign.readBoolean)
      # runExcept
      # Either.hush

  handle onChange = \event -> case readChecked event of
    Nothing -> Nothing
    Just _ -> Just $ onChange event
