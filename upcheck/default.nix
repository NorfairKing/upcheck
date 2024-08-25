{ mkDerivation, aeson, autodocodec, autodocodec-nix
, autodocodec-yaml, base, bytestring, genvalidity-bytestring
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, http-client, http-client-tls, http-types, iproute, lib
, network-uri, path, path-io, pretty-show, QuickCheck, retry
, sydtest, sydtest-discover, text, yaml
}:
mkDerivation {
  pname = "upcheck";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-nix autodocodec-yaml base bytestring
    http-client http-client-tls http-types network-uri path path-io
    pretty-show retry sydtest text yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-nix base genvalidity-bytestring genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text iproute QuickCheck
    sydtest text
  ];
  testToolDepends = [ sydtest-discover ];
  homepage = "https://github.com/NorfairKing/upcheck#readme";
  license = "unknown";
  mainProgram = "upcheck";
}
