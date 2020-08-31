module Demo.Utils where

import Protolude
import Web.HTML

import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Effect (Maybe HTMLElement)
selectElement query = do
  mel <- (querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window
  pure $ HTMLElement.fromElement =<< mel
