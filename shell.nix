{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, Earley, lens, mtl, process
      , stdenv, transformers
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
          base containers Earley lens mtl process transformers
        ];
        homepage = "https://github.com/githubuser/system-fc#readme";
        description = "Simple project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
