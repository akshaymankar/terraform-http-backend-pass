let sources = import ./nix/sources.nix;
in { pkgs ? import sources.nixpkgs {},
     compiler ? "ghc8103"
   }:
     let
       servantPkgs = import sources.servant {inherit pkgs compiler; };
       overrides = self: super: {
         terraform-http-backend-pass = self.callCabal2nix "terraform-http-backend-pass" ./. {};
         servant = servantPkgs.servant;
         servant-server = servantPkgs.servant-server;
       };
       hPkgs = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
     in hPkgs.terraform-http-backend-pass
