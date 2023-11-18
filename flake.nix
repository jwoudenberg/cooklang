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
      in {
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs."${python}"
            pkgs."${python}Packages".black
            pkgs."${python}Packages".flake8
          ];
        };
      });
}
