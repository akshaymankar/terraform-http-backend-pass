{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};
        hlib = pkgs.haskell.lib;
        # Avoids unnecessary recompiles
        filteredSource = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let baseName = baseNameOf (toString path);
            in pkgs.lib.cleanSourceFilter path type && !(
              baseName == "flake.nix" ||
              baseName == "flake.lock" ||
              baseName == "dist-newstyle" ||
              baseName == "nix" ||
              builtins.match "^cabal\.project\..*$" baseName != null ||
              baseName == ".envrc" ||
              baseName == "hie.yaml" ||
              baseName == ".hlint.yaml" ||
              baseName == ".hspec"
            );
        };
        haskellPackages = pkgs.haskell.packages.ghc94.override {
          overrides = hself: hsuper:
            {
              terraform-http-backend-pass = hlib.overrideSrc (hself.callPackage ./default.nix {}) {src = filteredSource;};
            };
        };
      in rec {
        packages = {
          terraform-http-backend-pass = haskellPackages.terraform-http-backend-pass;
          dev-shell = haskellPackages.shellFor {
            packages = p: [p.terraform-http-backend-pass];
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskell.packages.ghc94.haskell-language-server
              pkgs.cabal2nix

              pkgs.pass
            ];
          };
        };
        defaultPackage = packages.terraform-http-backend-pass;
        devShell = packages.dev-shell;
      });
}

