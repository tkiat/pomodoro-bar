{}:

let
  compiler = "ghc8107";

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "pomodoro-bar" =
        hself.callCabal2nix
          "pomodoro-bar"
          (gitignore ./.)
          { };
    };
  };

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."pomodoro-bar");

  docker = pkgs.dockerTools.buildImage {
    name = "pomodoro-bar";
    config.Cmd = [ "${exe}/bin/pomodoro-bar" ];
  };

in
{
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "pomodoro-bar" = myHaskellPackages."pomodoro-bar";
}
