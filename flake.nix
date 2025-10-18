{
  description = "A simple Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    ,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskell.compiler.ghc928;
        hs = pkgs.haskell.packages.ghc928;
      in
      rec {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.zlib
            pkgs.pkg-config
            ghc
            hs.haskell-language-server
            pkgs.haskellPackages.stack
            pkgs.haskellPackages.hlint
          ];
        };
      }
    );
}
