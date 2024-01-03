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

      templModeFor = emacsPkgs: emacsPkgs.trivialBuild {
        pname = "templ-ts-mode";
        version = "0";
        src = ./.;
      };
  in rec {
    packages = forAllSystems ({ system, pkgs }: {
      templ-grammar = pkgs.tree-sitter.buildGrammar {
        language = "tree-sitter-templ";
        version = "0.0";
        src = pkgs.fetchFromGitHub {
          owner = "vrischmann";
          repo = "tree-sitter-templ";
          rev = "f5c56b4739ea3303d125f6b7363645af631b85a1";
          sha256 = "sha256-FvjISUWLQJAQ20xTKlwXqqi+5FBKuCNMYI2JayGxObc=";
        };
      };

      templ-mode = templModeFor;

      templ-mode-emacs29 = templModeFor (pkgs.emacsPackagesFor pkgs.emacs29);
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
