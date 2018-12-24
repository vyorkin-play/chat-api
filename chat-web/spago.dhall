{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
	"chat"
, dependencies =
	[ "effect"
  , "aff"
  , "console"
  , "psci-support"
  , "unicode-prelude"
  , "simple-json"
  , "node-fs-aff"
  , "generics-rep-optics"
  , "halogen"
  , "halogen-css"
  , "svg-parser-halogen"
  , "css"
  , "affjax"
  , "parallel"
  , "tuples"
  , "either"
  , "now"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "quickcheck"
  , "test-unit"
  , "logging"
  , "remotedata"
  , "variant"
  , "nonempty"
  , "formatters"
  , "behaviors"
  ]
, packages =
	./packages.dhall
}
