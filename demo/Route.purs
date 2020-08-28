module Demo.Route where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex (RouteDuplex', root) as Routing.Duplex
import Routing.Duplex.Generic (noArgs, sum) as Routing.Duplex
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Index
  | Button

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where show = genericShow
instance encodeJsonRoute :: EncodeJson Route where encodeJson = genericEncodeJson
instance decodeJsonRoute :: DecodeJson Route where decodeJson = genericDecodeJson

-- where the key is an id of the page in the page manifest
-- pagesManifestRec
type PagesRec a =
  { "Index"  :: a
  , "Button" :: a
  }

-- NOTE: without a `Routing.Duplex.root $` to allow hashed routing
routeCodec :: Routing.Duplex.RouteDuplex' Route
routeCodec = Routing.Duplex.sum
  { "Index":  Routing.Duplex.noArgs
  , "Button": "button" / Routing.Duplex.noArgs
  }

extractFromPagesRec :: forall a . Route -> PagesRec a -> a
extractFromPagesRec Index  = _."Index"
extractFromPagesRec Button = _."Button"
