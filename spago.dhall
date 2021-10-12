{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "online-chess-enhancement-suite"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "signal"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
