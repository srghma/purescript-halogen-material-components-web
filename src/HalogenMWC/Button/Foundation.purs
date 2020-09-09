module HalogenMWC.Button.Foundation where

import Protolude
import Web.HTML.HTMLElement (HTMLElement)

data MDCRippleInstance

foreign import attachFoundation :: HTMLElement -> Effect MDCRippleInstance
