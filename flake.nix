{
  description = "parse-recipe";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        parse-recipe =
          pkgs.haskellPackages.callCabal2nix "parse-recipe" ./parse-recipe { };
      in {
        defaultPackage = pkgs.haskell.lib.justStaticExecutables parse-recipe;
        devShell =

          pkgs.haskellPackages.shellFor {
            packages = p: [ parse-recipe ];
            buildInputs = [
              pkgs.cabal-install
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.hpack
              pkgs.ormolu
              pkgs.swiProlog
            ];
          };
      });
}
