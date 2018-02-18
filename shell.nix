{ package ? "freq", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).freq
