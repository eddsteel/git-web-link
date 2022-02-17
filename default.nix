{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs.haskell.lib) doJailbreak;
  hpkgs = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      git-web-link = (hself.callPackage ./git-web-link.nix {}).overrideAttrs (attrs: {
        propagatedBuildInputs = attrs.propagatedBuildInputs ++ [ pkgs.git ];
        postConfigure = ''
          substituteInPlace src/GitWebLink/Paths.hs --replace \
            '"git"' '"${pkgs.git}/bin/git"'
        '';
      });
    };
  };
in hpkgs.git-web-link
