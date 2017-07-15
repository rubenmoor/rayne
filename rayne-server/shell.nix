with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        diener = self.callPackage ../../diener {};
        gerippe = self.callPackage ../../gerippe {};
        payne-server = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.payne-server.env
