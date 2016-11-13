{ mkDerivation, base, containers, directory, Earley, lens, mtl
, process, stdenv, transformers
}:
mkDerivation {
  pname = "dmenu";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers Earley lens mtl process transformers
  ];
  executableHaskellDepends = [
    base containers directory Earley lens mtl process transformers
  ];
  homepage = "https://github.com/githubuser/system-fc#readme";
  description = "Simple project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
