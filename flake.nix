{
  description = "Advent of Code 2022";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };
  outputs = { self, nixpkgs, devshell, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ devshell.overlay ]; };
        hPkgs = pkgs.haskell.packages.ghc902;
      in
      {
        devShells = rec {
          default = aoc2022;
          aoc2022 = pkgs.devshell.mkShell {
            imports = [ "${devshell}/extra/language/c.nix" ];
            name = "Advent of Code 2022";
            packages = with pkgs; [
              hPkgs.ghc
              hPkgs.ghcid
              hPkgs.ormolu
              hPkgs.hlint
              hPkgs.hoogle
              hPkgs.haskell-language-server
              hPkgs.implicit-hie
              hPkgs.retrie
              (pkgs.symlinkJoin {
                name = "stack";
                paths = [ pkgs.stack ];
                buildInputs = [ pkgs.makeWrapper ];
                postBuild = ''
                    wrapProgram $out/bin/stack \
                      --add-flags "\
                        --no-nix \
                        --system-ghc \
                        --no-install-ghc \
                      "
                '';
              })
            ];
            language.c = {
              compiler = pkgs.gcc;
              libraries = [ pkgs.zlib ];
            };
          };
        };
      }
    );
}
