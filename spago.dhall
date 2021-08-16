{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "halogen"
  , "identity"
  , "integers"
  , "lazy"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "psci-support"
  , "purescript-sequences"
  , "sdom"
  , "strings"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
