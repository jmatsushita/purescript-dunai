{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "dunai"
, dependencies =
  [ "effect"
  , "lists"
  , "monad-control"
  , "numerics"
  , "profunctor"
  , "safely"
  , "spec"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}