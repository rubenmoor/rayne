{ mkDerivation, aeson, base, base64-bytestring, bytestring
, case-insensitive, containers, data-default, diener
, email-validate, entropy, http-api-data, http-types, lens
, lifted-base, monad-control, mtl, opaleye, optparse-applicative
, parsec, postgresql-simple, product-profunctors, purescript-bridge
, pwstore-fast, servant, servant-purescript, servant-server
, servant-subscriber, stdenv, text, text-show, time, transformers
, transformers-base, wai, warp
}:
mkDerivation {
  pname = "payne-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive diener
    email-validate entropy http-api-data http-types lens lifted-base
    monad-control mtl opaleye parsec postgresql-simple
    product-profunctors pwstore-fast servant servant-purescript
    servant-server text text-show time transformers transformers-base
    wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers data-default diener http-api-data
    http-types lens mtl optparse-applicative postgresql-simple
    purescript-bridge servant servant-purescript servant-server
    servant-subscriber text text-show warp
  ];
  homepage = "";
  description = "server for my personal website, project name: payne";
  license = stdenv.lib.licenses.mit;
}
