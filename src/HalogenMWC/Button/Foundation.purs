module HalogenMWC.Button.Foundation where

import Protolude

import Effect.Uncurried as EFn
import Web.HTML.HTMLElement (HTMLElement)

data MDCRippleInstance

foreign import attachFoundation ::
  EFn.EffectFn3
  HTMLElement
  Boolean
  ( { addClass :: String -> Effect Unit -- (className) => options.root.classList.add(className)
    , root :: HTMLElement
    , removeClass :: String -> Effect Unit -- (className) => options.root.classList.remove(className)
    -- | NOTE:
    -- | $0.style.setProperty("--asdf", "qwe")
    -- | $0.style.setProperty("--asdf", "")
    -- | , updateCssVariable :: EFn.EffectFn2 String String Unit -- options.root.style.setProperty(varName, value)
    , isSurfaceDisabled :: Effect Boolean -- Boolean(instance.disabled)
    }
  )
  MDCRippleInstance
