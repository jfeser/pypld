{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, poetry2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (poetry2nix.legacyPackages.${system})
          mkPoetryApplication mkPoetryEnv;
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          pypld = mkPoetryApplication { projectDir = self; };
          default = self.packages.${system}.pypld;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.poetry (mkPoetryEnv { projectDir = self; }) ];
        };
      });
}
