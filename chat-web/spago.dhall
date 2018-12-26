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
  , "halogen-formless"
  , "svg-parser-halogen"
  , "css"
  , "affjax"
  , "parallel"
  , "tuples"
  , "either"
  , "web-socket"
  , "coroutines"
  , "aff-coroutines"
  , "now"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
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
