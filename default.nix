{ mkDerivation, aeson, base, bytestring, directory, lib, mtl
, optparse-applicative, optparse-generic, servant, servant-server
, shelly, text, warp
}:
mkDerivation {
  pname = "terraform-http-backend-pass";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring directory mtl optparse-applicative
    optparse-generic servant servant-server shelly text warp
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/akshaymankar/terraform-http-backend-pass#readme";
  description = "HTTP backend to store terraform state using pass and git";
  license = "AGPL";
}
