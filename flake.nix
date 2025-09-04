{
  description = "eio-trace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};
    ocamlPackages = pkgs.ocamlPackages;
    processor = ocamlPackages.buildDunePackage rec {
      pname = "processor";
      version = "0.2";
      src = pkgs.fetchFromGitHub {
        owner = "haesbaert";
        repo = "ocaml-processor";
        name = pname;
        tag = version;
        hash = "sha256-0jxi3Qz1nlnClPQ6Za0vFBig4ahrkyezicyqmErx1QE=";
      }; 
      buildInputs = with ocamlPackages; [ocaml dune_3];
    }; in {
      packages.default = ocamlPackages.buildDunePackage {
        pname = "eio-trace";
        minimalOCamlVersion = "5.1";
        version = "0.4";
        src = ./.;
        buildInputs = with ocamlPackages; [dune_3 ocaml processor eio_main cmdliner lablgtk3];
      };
    });
  }
