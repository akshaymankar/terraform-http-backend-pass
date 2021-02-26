{ mkDerivation, base, bytestring, mtl, optparse-applicative
, optparse-generic, servant, servant-server, shelly, stdenv, text
, warp, zlib
}:
mkDerivation {
  pname = "terraform-http-backend-pass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring mtl optparse-applicative optparse-generic servant
    servant-server shelly text warp
  ];
  librarySystemDepends = [ zlib ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
