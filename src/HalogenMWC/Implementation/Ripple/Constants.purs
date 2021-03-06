module HalogenMWC.Implementation.Ripple.Constants where


import Web.Event.Event (EventType(..))

strings ::
  { "VAR_FG_SCALE"           :: String
  , "VAR_FG_SIZE"            :: String
  , "VAR_FG_TRANSLATE_END"   :: String
  , "VAR_FG_TRANSLATE_START" :: String
  , "VAR_LEFT"               :: String
  , "VAR_TOP"                :: String
  }
strings =
  { "VAR_FG_SCALE":           "--mdc-ripple-fg-scale"
  , "VAR_FG_SIZE":            "--mdc-ripple-fg-size"
  , "VAR_FG_TRANSLATE_END":   "--mdc-ripple-fg-translate-end"
  , "VAR_FG_TRANSLATE_START": "--mdc-ripple-fg-translate-start"
  , "VAR_LEFT":               "--mdc-ripple-left"
  , "VAR_TOP":                "--mdc-ripple-top"
  }

numbers ::
  { "FG_DEACTIVATION_MS"      :: Int -- Corresponds to $mdc-ripple-fade-out-duration (i.e. deactivation animation duration)
  -- | , "DEACTIVATION_TIMEOUT_MS" :: Int -- Corresponds to $mdc-ripple-translate-duration (i.e. activation animation duration)
  , "INITIAL_ORIGIN_SCALE"    :: Number
  , "PADDING"                 :: Number
  -- | , "TAP_DELAY_MS"            :: Int -- Delay between touch and simulated mouse events on touch devices
  }
numbers =
  { "FG_DEACTIVATION_MS":          1500
  -- | , "DEACTIVATION_TIMEOUT_MS":     225
  , "INITIAL_ORIGIN_SCALE":        0.6
  , "PADDING":                     10.0
  -- | , "TAP_DELAY_MS":                300
  }

pointer_deactivation_event_types :: Array EventType
pointer_deactivation_event_types =
  [ EventType "touchend"
  , EventType "pointerup"
  , EventType "mouseup"
  , EventType "contextmenu"
  ]
