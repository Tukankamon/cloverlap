{
	inputs = {
    #nixpkgs.url = "nixpkgs/nixos-25.11";
    nixpkgs.url = "nixpkgs/nixos-unstable";
		# Easier elm packaging
		mkElmDerivation = {
			url = "github:jeslie0/mkElmDerivation";
			inputs.nixpkgs.follows = "nixpkgs";
		};
	};

  outputs = { self, nixpkgs, mkElmDerivation}:
    let
			system = "x86_64-linux";
      pkgs = import nixpkgs {
        overlays = [ mkElmDerivation.overlays.default ];
        inherit system;
      };
      hpkgs = pkgs.haskellPackages;

      elm-app = pkgs.mkElmDerivation {
        name = "cloverlap-elm";
        src = ./frontend;
        buildInputs = [ pkgs.elmPackages.elm ];
        buildPhase = ''
          export HOME=$TMPDIR
          elm make src/Main.elm --output=static/app.js --optimize
        '';
        installPhase = ''
          mkdir -p $out/static
          cp -r static/* $out/static/
          cp index.html $out/index.html
        '';
      };

			cabal-pkg = name: (hpkgs.callCabal2nix "" ./. {}).overrideAttrs (old: {
				meta = (old.meta or {}) // { mainProgram = name; };
				configureflags = (old.configureFlags or []) ++ [ "--ghc-option=-O2" ];
				enableParallelBuilding = true;
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.makeWrapper ];
        postInstall = ''
					mkdir -p $out/frontend/static
          cp -r ${elm-app}/static $out/frontend/
          cp ${elm-app}/index.html $out/frontend/index.html
					# The wrap is needed for scotty to find the files
          wrapProgram $out/bin/server \
            --chdir $out
        '';
			});

    in
    {
      devShells.${system}.default = pkgs.mkShell {
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
					#elm-format
					elm-language-server
					elm-review
        ]);
        shellHook = ''
					exec fish
          #set SHELL $(which fish)
        '';
      };

		packages.${system} = {
			cli = cabal-pkg "cli";
			# Doesnt work right now since it doesnt have access to the elm site
			server = cabal-pkg "server";
			default = cabal-pkg "cli";
		};
	};
}
