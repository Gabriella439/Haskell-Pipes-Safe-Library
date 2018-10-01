{ mkDerivation, base, containers, exceptions, monad-control, mtl
, pipes, primitive, stdenv, transformers, transformers-base
}:
mkDerivation {
  pname = "pipes-safe";
  version = "2.3.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers exceptions monad-control mtl pipes primitive
    transformers transformers-base
  ];
  description = "Safety for the pipes ecosystem";
  license = stdenv.lib.licenses.bsd3;
}
