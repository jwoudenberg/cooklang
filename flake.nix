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
        python = "python3";
        ppkgs = pkgs."${python}Packages";

        cooklang_lib = ppkgs.buildPythonPackage {
          pname = "cooklang";
          version = "1.0.0";
          format = "pyproject";
          src = ./.;
          nativeBuildInputs = [ ppkgs.setuptools ];
        };
        cooklang_pkg = ppkgs.toPythonApplication cooklang_lib;
        cooklang_app = {
          type = "app";
          program = "${cooklang_pkg}/bin/cooklang";
        };
      in
      {
        python3Packages.cooklang = cooklang_lib;
        packages.cooklang = cooklang_pkg;
        defaultPackage = cooklang_pkg;
        apps.cooklang = cooklang_app;
        defaultApp = cooklang_app;

        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.haskellPackages.shelltestrunner
            pkgs.html-tidy
            pkgs."${python}"
            ppkgs.black
            ppkgs.flake8
          ];
        };
      });
}
