module Demo.Utils where

import Protolude (Effect, Maybe, Unit, bind, const, error, maybe, pure, throwError, unit, ($), (<<<), (<=<), (<>), (=<<), (>>=))
import Web.HTML (HTMLElement, window)
import Halogen as H
import Halogen.HTML as HH
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus) as Web.HTML.HTMLElement
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Effect (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window
  pure $ HTMLElement.fromElement =<< mel

focusById :: String -> Effect Unit
focusById id = do
  (htmlElement :: HTMLElement) <- selectElement (QuerySelector ("#" <> id)) >>= maybe (throwError $ error $ "cannot find required element by id: " <> id) pure
  Web.HTML.HTMLElement.focus htmlElement

mkComponentStatic :: forall query input output action slots m. HH.ComponentHTML action slots m -> H.Component query input output m
mkComponentStatic render =
  H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval H.defaultEval
    }
