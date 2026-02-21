{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/cf3f5c4def3c7b5f1fc012b3d839575dbe552d43";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.instaepub =
      nixpkgs.legacyPackages.x86_64-linux.haskellPackages.callPackage ./package.nix {};

    packages.x86_64-linux.default = self.packages.x86_64-linux.instaepub;

    devShells.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.shellFor {
        packages = _: [ self.packages.x86_64-linux.instaepub ];
        nativeBuildInputs = [ haskell-language-server cabal2nix cabal-install ];
      };

  };
}
