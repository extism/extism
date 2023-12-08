{  
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, naersk, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
        };

        naersk' = pkgs.callPackage naersk {};

      in {
        # For `nix build` & `nix run`:
        defaultPackage = naersk'.buildPackage {
          name = "libextism";
          src = ./.;
          copyLibs = true;
          postInstall = "mkdir -p $out/include; cp runtime/extism.h $out/include";
        };

        # For `nix develop`:
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ rustc cargo clippy gnumake ];
        };
      }
    );
}
