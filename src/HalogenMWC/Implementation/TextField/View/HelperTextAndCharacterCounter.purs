module HalogenMWC.Implementation.TextField.View.HelperTextAndCharacterCounter where

import HalogenMWC.Implementation.TextField.View.Shared
import Material.Classes.Textfield
import Protolude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Halogen (AttrName(..), ClassName)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HP.ARIA
import HalogenMWC.Implementation.TextField.View.CharacterCounter as CharacterCounter
import HalogenMWC.Implementation.TextField.View.CharacterCounter (CharacterCounterConfig)
import HalogenMWC.Implementation.TextField.View.HelperText (HelperTextConfig)
import HalogenMWC.Implementation.TextField.View.HelperText as HelperText
import HalogenMWC.Implementation.TextField.View.FilledShared as FilledShared
import HalogenMWC.Implementation.TextField.View.OutlinedShared as OutlinedShared
import HalogenMWC.Implementation.TextField.View.HelperLine as HelperLine


renderBoth
  :: forall w i r
   . { helperText       :: Maybe HelperTextConfig
     , characterCounter :: Maybe CharacterCounterConfig
     | r
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

wrapRenderBoth
  :: ∀ r i w
  . ({ characterCounter ∷ Maybe CharacterCounterConfig , helperText ∷ Maybe HelperTextConfig | r } → HH.HTML w i)
  → { characterCounter ∷ Maybe CharacterCounterConfig , helperText ∷ Maybe HelperTextConfig | r }
  → Array (HH.HTML w i)
wrapRenderBoth renderInternal config = [ renderInternal config ] <> renderBoth config
