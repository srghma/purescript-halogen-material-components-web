module HalogenMWC.Utils where

import Prelude (const, ($), (<<<), (<=<), (>=>))
import Halogen (ClassName, PropName)

import Halogen.HTML.Core (Prop(..), PropValue)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType)
import Web.Event.Event as EE
import Foreign (F, Foreign, unsafeToForeign)
import Foreign.Index (readProp)
import Halogen.HTML.Events as HE
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Halogen.HTML.Properties (IProp)

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
addForeignPropHandler key prop_ reader f =
  HE.handler' key $ EE.currentTarget >=> \e -> either (const Nothing) (Just <<< f) $ runExcept $ go e
  where
  go a = reader <=< readProp prop_ $ unsafeToForeign a
