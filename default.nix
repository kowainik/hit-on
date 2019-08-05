{ # The git revision here corresponds to the nixpkgs-unstable channel, which at
  # the time of this writing has GHC 8.6.5 as the default compiler (matching the
  # one used by stack.yaml). Use https://howoldis.herokuapp.com/ to determine
  # the current rev.
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/002b853782e.tar.gz") {}
  # Which GHC compiler to use.
  # To determine the list of compilers available run:
  #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
, compiler ? "default"
}:
let
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
in
haskellPackages.developPackage {
  # The path to our cabal project's root directory
  root = ./.;

  # Haskell packages to override
  source-overrides = {
    github = builtins.fetchTarball "https://github.com/phadej/github/archive/cdae698f50f9e6dc1b58d2181672294e2b11dfb3.tar.gz";
    relude = builtins.fetchTarball "https://github.com/kowainik/relude/archive/55968311244690f5cc8b4484a37a63d988ea2ec4.tar.gz";
    shellmet = builtins.fetchTarball "https://github.com/kowainik/shellmet/archive/36149eb0eb2b81916a93cdb92f3cb949d2eb9d23.tar.gz";
  };

  overrides = self: super: with pkgs.haskell.lib; {
    github = dontCheck super.github;  # `dontCheck` skips running tests on this package
  };
}
