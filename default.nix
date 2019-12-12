{}:
let 
  config = {
      packageOverrides = pkgs: rec {
          haskellPackages = pkgs.haskellPackages.override {
              overrides = haskellPackagesNew: haskellPackagesOld: rec {
                 worlds-downloader = haskellPackagesOld.callCabal2nix "worlds-downloader" ./. {};
              };
          };
      };
  };

  pkgs = import <nixpkgs> { inherit config; };

in pkgs