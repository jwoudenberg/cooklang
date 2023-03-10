{
  description = "groceries";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        python = "python310";
        cooklang-to-html = pkgs.stdenv.mkDerivation {
          name = "cooklang-to-html";
          propagatedBuildInputs = [ pkgs."${python}" ];
          dontUnpack = true;
          src = ./src;
          installPhase = ''
            mkdir -p $out/bin $out/lib
            install -D -m=755 $src/* $out/lib
            ln -s $out/lib/cooklang-to-html $out/bin/cooklang-to-html
          '';
        };
      in {
        apps.cooklang-to-html = {
          type = "app";
          program = "${cooklang-to-html}/bin/cooklang-to-html";
        };
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.haskellPackages.shelltestrunner
            pkgs.html-tidy
            pkgs."${python}"
            pkgs."${python}Packages".black
            pkgs."${python}Packages".flake8
          ];
        };
      });
}
