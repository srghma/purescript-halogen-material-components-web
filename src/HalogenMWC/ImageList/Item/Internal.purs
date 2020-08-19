module HalogenMWC.ImageArray.Item.Internal (Config(..), ImageArrayItem(..))

import Html


type Config r i
    = Config
        { label :: Maybe String
        , href :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , image :: String
        }


data ImageArrayItem msg
    = ImageArrayItem (Config msg)
