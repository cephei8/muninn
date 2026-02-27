{
  description = "muninn";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    odin-src = { url = "github:odin-lang/Odin/dev-2026-02"; flake = false; };
    ols-src = { url = "github:DanielGavin/ols/dev-2026-02"; flake = false; };
  };

  outputs = { self, nixpkgs, flake-utils, odin-src, ols-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.haskell.lib.dontCheck
          (pkgs.haskellPackages.callCabal2nix "muninn" self { });

        packages.testgen-parser = pkgs.stdenv.mkDerivation {
          name = "testgen-parser";
          src = ./testgen/parser;
          nativeBuildInputs = [ pkgs.odin ];
          buildPhase = ''
            odin build . -o:none -out:testgen
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp testgen $out/bin/testgen-parser
          '';
        };

        apps.testgen-parser = {
          type = "app";
          program = toString (pkgs.writeShellScript "testgen-parser" ''
            ${self.packages.${system}.testgen-parser}/bin/testgen-parser generate "${odin-src}" test/parser/golden-stdlib core vendor base
            ${self.packages.${system}.testgen-parser}/bin/testgen-parser generate "${ols-src}" test/parser/golden-ols src tests tools builtin
          '');
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install

            haskell-language-server
            fourmolu

            odin
          ];

          ODIN_SRC_TESTDATA = "${odin-src}";
          OLS_SRC_TESTDATA = "${ols-src}";
        };
      }
    );
}
