
{}:

let pkgs = import ./. {};

in 
  pkgs.haskellPackages.shellFor {
      packages = (ps: [ps.worlds-downloader]);
      buildInputs = [pkgs.cabal-install pkgs.ghcid];
      withHoogle = true;
  }
  