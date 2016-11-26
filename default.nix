{ mkDerivation, base, containers, directory, lens, mtl, process
, stdenv, transformers
}:
mkDerivation {
  pname = "dmenu";
  version = "0.3.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers directory lens mtl process transformers
  ];
  homepage = "https://github.com/m0rphism/haskell-dmenu";
  description = "Complete bindings to the dmenu and dmenu2 command line tools";
  license = stdenv.lib.licenses.bsd3;
}
