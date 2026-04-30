{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/15f4ee454b1dce334612fa6843b3e05cf546efab";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.instaepub =
      let hspkgs = nixpkgs.legacyPackages.x86_64-linux.haskellPackages; in
      hspkgs.callPackage ./package.nix { scotty = hspkgs.scotty_0_30; };

    packages.x86_64-linux.default = self.packages.x86_64-linux.instaepub;

    devShells.x86_64-linux.default =
      with nixpkgs.legacyPackages.x86_64-linux;
      haskellPackages.shellFor {
        packages = _: [ self.packages.x86_64-linux.instaepub ];
        nativeBuildInputs = [ haskell-language-server cabal2nix cabal-install ];
      };

  };
}
