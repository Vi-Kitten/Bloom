{
    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    };

    outputs = { self, nixpkgs }: let
        pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in {
        devShells."x86_64-linux".default = pkgs.mkShell {
            buildInputs = with pkgs; [
                haskell-language-server
                (ghc.withPackages (hsPkgs: with hsPkgs; [
                    cabal-install
                    turtle      # Faster startup time with all external shell commands
                    shh         # Piping operators and other goodies
                    shh-extras  # Try shh as an interactive shell
                ]))
            ];
        };
    };
}
