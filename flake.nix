{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        buildDeps =
          [ pkgs.graphviz-nox pkgs.python310Packages.graphviz pkgs.python3 ];
        devDeps = buildDeps ++ [
          pkgs.black
          pkgs.python310Packages.hypothesis
          pkgs.python310Packages.pytest
          pkgs.python310Packages.python-lsp-server
          pkgs.python310Packages.pylsp-mypy
        ];
      in {
        devShells.default = pkgs.mkShell { nativeBuildInputs = devDeps; };
      });
}
