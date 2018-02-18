{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "discrete-intervals";
  version = "0.0.3";
  src = fetchgit {
    url = "https://github.com/chessai/discrete-intervals.git";
    sha256 = "1xq1lsmn2p614qnbk763zf2m3n6hbjshj72xn57xmb5jd9j592ms";
    rev = "9173b1306ee7233e0efa0553ec321b8f1bdfea7b";
  };
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/chessai/discrete-intervals";
  description = "Discrete Intervals";
  license = stdenv.lib.licenses.bsd3;
}
