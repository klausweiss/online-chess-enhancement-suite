module OCES.Keyboard where

import Prelude

import Data.Array (range)

type Keycode = Int

-- todo
keycodeFor :: Char -> Keycode
keycodeFor = keycodeFor'
  where
   keycodeFor' 'a' = 65
   keycodeFor' 'b' = 66
   keycodeFor' 'c' = 67
   keycodeFor' 'd' = 68
   keycodeFor' 'e' = 69
   keycodeFor' 'f' = 70
   keycodeFor' 'g' = 71
   keycodeFor' 'h' = 72
   keycodeFor' 'i' = 73
   keycodeFor' 'j' = 74
   keycodeFor' 'k' = 75
   keycodeFor' 'l' = 76
   keycodeFor' 'm' = 77
   keycodeFor' 'n' = 78
   keycodeFor' 'o' = 79
   keycodeFor' 'p' = 80
   keycodeFor' 'q' = 81
   keycodeFor' 'r' = 82
   keycodeFor' 's' = 83
   keycodeFor' 't' = 84
   keycodeFor' 'u' = 85
   keycodeFor' 'v' = 86
   keycodeFor' 'w' = 87
   keycodeFor' 'x' = 88
   keycodeFor' 'y' = 89
   keycodeFor' 'z' = 90
   keycodeFor' ' ' = 32
   keycodeFor' _   = -1

toHumanReadable :: Keycode -> String
toHumanReadable = toHumanReadable'
  where
    toHumanReadable' 65 = "a"
    toHumanReadable' 66 = "b"
    toHumanReadable' 67 = "c"
    toHumanReadable' 68 = "d"
    toHumanReadable' 69 = "e"
    toHumanReadable' 70 = "f"
    toHumanReadable' 71 = "g"
    toHumanReadable' 72 = "h"
    toHumanReadable' 73 = "i"
    toHumanReadable' 74 = "j"
    toHumanReadable' 75 = "k"
    toHumanReadable' 76 = "l"
    toHumanReadable' 77 = "m"
    toHumanReadable' 78 = "n"
    toHumanReadable' 79 = "o"
    toHumanReadable' 80 = "p"
    toHumanReadable' 81 = "q"
    toHumanReadable' 82 = "r"
    toHumanReadable' 83 = "s"
    toHumanReadable' 84 = "t"
    toHumanReadable' 85 = "u"
    toHumanReadable' 86 = "v"
    toHumanReadable' 87 = "w"
    toHumanReadable' 88 = "x"
    toHumanReadable' 89 = "y"
    toHumanReadable' 90 = "z"
    toHumanReadable' 32 = "space"
    toHumanReadable' 16 = "shift"
    toHumanReadable' 17 = "ctrl"
    toHumanReadable' 18 = "alt"
    toHumanReadable' 27 = "esc"
    toHumanReadable' kc = "unsupported key with keycode: " <> show kc

shiftKey :: Keycode
shiftKey = 16

ctrlKey :: Keycode
ctrlKey = 17

altKey :: Keycode
altKey = 18

escKey :: Keycode
escKey = 27

supportsKeyUp :: Keycode -> Boolean
supportsKeyUp k | k == shiftKey = false
supportsKeyUp _ = true

supportsKeyDown :: Keycode -> Boolean
supportsKeyDown k | k == escKey = false
supportsKeyDown _ = true

supportedKeycodes :: Array Keycode
supportedKeycodes = 
  let letters = range 65 90
      space = keycodeFor ' '
   in
   letters <> [space, shiftKey, ctrlKey, altKey, escKey]
