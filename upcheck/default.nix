{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base
, bytestring, http-client, http-client-tls, http-types, lib
, network-uri, path, path-io, pretty-show, retry, sydtest, text
, yaml
}:
mkDerivation {
  pname = "upcheck";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base bytestring http-client
    http-client-tls http-types network-uri path path-io pretty-show
    retry sydtest text yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/upcheck#readme";
  license = "unknown";
  mainProgram = "upcheck";
}
