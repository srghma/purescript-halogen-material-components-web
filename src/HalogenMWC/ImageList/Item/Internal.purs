module HalogenMWC.ImageArray.Item.Internal (Config(..), ImageArrayItem(..)) where

import Html


type Config r i
    = Config
        { label :: Maybe String
        , href :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , image :: String
        }


data ImageArrayItem r i
    = ImageArrayItem (Config r i)
