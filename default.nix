{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, byteable, bytestring, case-insensitive, conduit, containers
, cryptohash, data-default, failure, hashable, hspec, HTTP
, http-conduit, http-types, network, old-locale, stdenv, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "github";
  version = "0.14.0";
  src = ./.;
  buildDepends = [
    aeson attoparsec base base16-bytestring byteable bytestring
    case-insensitive conduit containers cryptohash data-default failure
    hashable HTTP http-conduit http-types network old-locale text time
    unordered-containers vector
  ];
  testDepends = [
    aeson attoparsec base base16-bytestring byteable bytestring
    case-insensitive conduit containers cryptohash data-default failure
    hashable hspec HTTP http-conduit http-types network old-locale text
    time unordered-containers vector
  ];
  homepage = "https://github.com/fpco/github";
  description = "Access to the Github API, v3";
  license = stdenv.lib.licenses.bsd3;
}
