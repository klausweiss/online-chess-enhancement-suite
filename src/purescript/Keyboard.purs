module Keyboard where

import Prelude (negate)

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
   keycodeFor' 'z' = 88
   keycodeFor' ' ' = 32
   keycodeFor' _   = -1

shiftKey :: Keycode
shiftKey = 16

ctrlKey :: Keycode
ctrlKey = 17
