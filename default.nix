{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, either, exceptions, free, HList, lens, mmorph, mtl, opaleye
, opaleye-sot, pipes, pipes-concurrency, postgresql-simple
, scintilla, stdenv, tasty, tasty-hspec, tasty-quickcheck, text
, thyme, time, transformers, unordered-containers, uuid
}:
mkDerivation {
  pname = "scintilla-todo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers either exceptions free
    HList lens mmorph mtl opaleye opaleye-sot pipes pipes-concurrency
    postgresql-simple scintilla text thyme time transformers
    unordered-containers uuid
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base containers mmorph scintilla tasty tasty-hspec tasty-quickcheck
    unordered-containers
  ];
  homepage = "http://github.com/cdodev/scinitilla-example";
  description = "Example scintilla app";
  license = stdenv.lib.licenses.bsd3;
}
