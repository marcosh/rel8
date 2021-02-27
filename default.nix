let 
  haskellNix = import (import ./nix/sources.nix)."haskell.nix" {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;

  nixpkgsArgs = haskellNix.nixpkgsArgs;

  compiler-nix-name = "ghc8103";

  pkgs = import nixpkgsSrc nixpkgsArgs;

in 
pkgs.haskell-nix.project {
  inherit compiler-nix-name;

  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "rel8";
    src = ./.;
  };
}
