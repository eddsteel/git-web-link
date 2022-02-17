{ mkDerivation, ansi-wl-pprint, base, containers, directory
, doctest, filepath, hpack, lib, network-uri, open-browser
, optparse-applicative, regex-base, regex-tdfa, safe, text
, transformers, turtle, url
}:
mkDerivation {
  pname = "git-web-link";
  version = "0.9";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory filepath network-uri regex-base
    regex-tdfa safe text transformers turtle url
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    ansi-wl-pprint base containers directory filepath network-uri
    open-browser optparse-applicative regex-base regex-tdfa safe text
    transformers turtle url
  ];
  testHaskellDepends = [
    base containers directory doctest filepath network-uri regex-base
    regex-tdfa safe text transformers turtle url
  ];
  prePatch = "hpack";
  homepage = "https://github.com/eddsteel/git-web-link#readme";
  license = lib.licenses.gpl3Only;
}
