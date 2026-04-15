{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages;
			cabal-pkg = hpkgs.callCabal2nix "" ./. {};
    in
    {
      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = (with pkgs; [
					zlib # cabal complained without this
          ghc # Compiler
          fish # Better than bash (default shell)
          cabal-install
					hlint
          haskell-language-server
				]) ++
				(with pkgs.elmPackages; [
					elm
					elm-format
					elm-language-server
					elm-review
        ]);
        shellHook = ''
					fish
					set SHELL $(which fish)
        '';
      };

		packages.x86_64-linux = {
			cli = cabal-pkg;
				#server = cabal-pkg.components.exes.cloverlap-server;
			default = cabal-pkg;
		};
	};
}
