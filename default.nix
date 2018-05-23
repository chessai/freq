{ package ? "freq", compiler ? "ghc842" }:
let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "1e225e6481530170068103d3916108ba5ee7a9d3"; 
      sha256 = "xxa7bk1awgl2ndndakc280riq8sj6m0gifl46v0xq034pf8mnliw"; 
      sha256unpacked = "0zzi0sv4a156qkbp3xhb85d5vm127kcxgmsqhfwvwgmlz8fidlnd"; 
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;
 
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };
    {
      freq = build "freq" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  freq = drv;
}
