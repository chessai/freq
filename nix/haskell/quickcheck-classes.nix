{ mkDerivation, aeson, base, fetchgit, prim-array, primitive
, QuickCheck, stdenv, transformers, vector
}:
mkDerivation {
  pname = "quickcheck-classes";
  version = "0.3.1";
  src = fetchgit {
    url = "https://github.com/andrewthad/quickcheck-classes.git";
    sha256 = "0wsyqfaa5r0ki9j69aaabnyawa3n6zxasvhag84xc493b5l8db4g";
    rev = "a31ef48abba4bc6c94f783f472235455c4800da7";
  };
  libraryHaskellDepends = [
    aeson base prim-array primitive QuickCheck transformers
  ];
  testHaskellDepends = [ aeson base primitive QuickCheck vector ];
  homepage = "https://github.com/andrewthad/quickcheck-classes#readme";
  description = "QuickCheck common typeclasses";
  license = stdenv.lib.licenses.bsd3;
}
