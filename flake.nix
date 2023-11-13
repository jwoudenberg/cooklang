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
        ppkgs = pkgs."${python}Packages";

        cooklang_to_html_lib = ppkgs.buildPythonPackage {
          pname = "cooklang_to_html";
          version = "1.0.0";
          format = "pyproject";
          src = ./.;
          nativeBuildInputs = [ ppkgs.setuptools ];
        };
        cooklang_to_html_pkg = ppkgs.toPythonApplication cooklang_to_html_lib;
        cooklang_to_html_app = {
          type = "app";
          program = "${cooklang_to_html_pkg}/bin/cooklang-to-html";
        };
      in
      {
        python3Packages.cooklang_to_html = cooklang_to_html_lib;
        packages.cooklang-to-html = cooklang_to_html_pkg;
        defaultPackage = cooklang_to_html_pkg;
        apps.cooklang-to-html = cooklang_to_html_app;
        defaultApp = cooklang_to_html_app;

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
