{
  description = "A feature-rich CLI-based Pomorodo clock with optional integration with external displays: currently polybar and xmobar.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs }:
    let
      compiler = "ghc8107";

      myHaskellPackages.x86_64-linux =
        with import nixpkgs { system = "x86_64-linux"; };
        let
          gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        in
        pkgs.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: {
            "pomodoro-bar" =
              hself.callCabal2nix
                "pomodoro-bar"
                (gitignore ./.)
                { };
          };
        };
    in
    {
      defaultPackage.x86_64-linux =
        with import nixpkgs { system = "x86_64-linux"; };
        pkgs.haskell.lib.justStaticExecutables
          (myHaskellPackages.x86_64-linux."pomodoro-bar");

      devShell.x86_64-linux =
        with import nixpkgs { system = "x86_64-linux"; };
        myHaskellPackages.x86_64-linux.shellFor {
          shellHook = ''
            export PS1="\e[01;32mnix-develop\e[m\e[01;31m (pomodoro-bar)\e[m\$ "
          '';
          packages = p: [
            p."pomodoro-bar"
          ];
          buildInputs = [
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.implicit-hie
            pkgs.haskellPackages.ormolu
            pkgs.nixpkgs-fmt
          ];
          withHoogle = true;
        };
    };
}
