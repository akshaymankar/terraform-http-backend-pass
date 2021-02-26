let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in {
  env = pkgs.buildEnv {
    name = "terraform-http-pass-backend";
    paths = with pkgs; [
      pass
      niv
      gnumake
      haskell-language-server
      cabal-install
      haskell.compiler.ghc8103 # HLS doesn't support GHC 9 yet.
      zlib.dev
      zlib
    ];
  };
}
