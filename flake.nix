{
  description = "Emacs major mode for editing Templ files";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    templ = {
      url = github:a-h/templ;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, templ }: let
      allSystems = [
        "x86_64-linux" # 64-bit Intel/AMD Linux
        "aarch64-linux" # 64-bit ARM Linux
        "x86_64-darwin" # 64-bit Intel macOS
        "aarch64-darwin" # 64-bit ARM macOS
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
        inherit system;
        pkgs = import nixpkgs { inherit system; };
      });

      grammarRev = "592faa3186ef857c92e4bd1c31d73c07a4a334db";
  in {
    packages = forAllSystems ({ system, pkgs }: rec {
      default = templ-mode-emacs29;

      templ-grammar = pkgs.tree-sitter.buildGrammar {
        language = "tree-sitter-templ";
        version = grammarRev;
        src = pkgs.fetchFromGitHub {
          owner = "vrischmann";
          repo = "tree-sitter-templ";
          rev = grammarRev;
          sha256 = "sha256-XX1+P8ibo8REYYZQaC47lneg/roralo+YiRwFNnARsQ=";
        };
      };

      templ-mode = emacsPkgs: emacsPkgs.trivialBuild {
        pname = "templ-ts-mode";
        version = "0";
        src = ./.;
      };

      templ-mode-emacs29 = templ-mode (pkgs.emacsPackagesFor pkgs.emacs29);
    });

    devShell = forAllSystems ({ system, pkgs }:
      pkgs.mkShell {
        buildInputs = [
          templ.packages.${system}.templ
          templ.packages.${system}.templ-docs
        ];
      });
  };
}
