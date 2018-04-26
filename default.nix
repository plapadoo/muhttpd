{ mkDerivation, base, bytestring, case-insensitive, http-types
, optparse-applicative, stdenv, wai, warp
}:
mkDerivation {
  pname = "muhttpd";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring case-insensitive http-types optparse-applicative
    wai warp
  ];
  homepage = "https://github.com/plapadoo/muhttpd";
  description = "Small, Unix-philosophy HTTP server";
  license = stdenv.lib.licenses.gpl3;
}
