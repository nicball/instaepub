{
  description = "";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/dd9b079222d43e1943b6ebd802f04fd959dc8e61";

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
