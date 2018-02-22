{ package ? "freq", compiler ? "ghc822" }:

(import ./nix/default.nix {
  inherit package compiler;
}).freq
