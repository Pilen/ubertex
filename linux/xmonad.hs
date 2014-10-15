import XMonad
import XMonad.Layout.NoBorders

main = xmonad $ defaultConfig {
  terminal = "urxvt",
  borderWidth = 1,
  normalBorderColor  = "#000000",
  focusedBorderColor = "#000000",
  modMask = mod4Mask, -- Set win as mod key
  layoutHook = smartBorders $ layoutHook defaultConfig
  }
