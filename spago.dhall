{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "console"
    , "effect"
    , "generics-rep"
    , "halogen"
    , "js-date"
    , "psci-support"
    , "remotedata"
    , "tolerant-argonaut"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
