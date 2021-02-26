{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, directory, mtl
      , optparse-applicative, optparse-generic, servant, servant-server
      , shelly, stdenv, text, warp, zlib
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
        librarySystemDepends = [ zlib ];
        executableHaskellDepends = [ base ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
