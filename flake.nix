{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages;
    in
    {
      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = with pkgs; [
          ghc # Compiler
          fish # Better than bash (default shell)
          cabal-install
          haskell-language-server
          hpkgs.tinyfiledialogs
        ];
        shellHook = ''
          fish
        '';
      };

      # Proof.hs
      packages."x86_64-linux".default =
        hpkgs.callCabal2nix "" ./. {};
    };
}
