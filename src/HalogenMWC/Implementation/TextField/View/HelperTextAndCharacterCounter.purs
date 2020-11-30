module HalogenMWC.Implementation.TextField.View.HelperTextAndCharacterCounter where

import Protolude

import Data.Array as Array
import Halogen.HTML as HH
import HalogenMWC.Implementation.TextField.View.CharacterCounter as CharacterCounter
import HalogenMWC.Implementation.TextField.View.CharacterCounter (CharacterCounterConfig)
import HalogenMWC.Implementation.TextField.View.HelperText (HelperTextConfig)
import HalogenMWC.Implementation.TextField.View.HelperText as HelperText
import HalogenMWC.Implementation.TextField.View.HelperLine as HelperLine

renderBoth
  :: forall w i
   . { helperText       :: Maybe HelperTextConfig
     , characterCounter :: Maybe CharacterCounterConfig
     }
  -> Array (HH.HTML w i)
renderBoth { helperText, characterCounter } =
  case helperText, characterCounter of
       Nothing, Nothing -> []
       _, _ ->
         [ HelperLine.helperLine $ Array.catMaybes
            [ map CharacterCounter.characterCounter characterCounter
            , map HelperText.helperText helperText
            ]
         ]
