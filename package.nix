{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, file-embed, http-client, http-client-tls, http-types, lib
, modern-uri, monad-logger, pandoc, pandoc-types, persistent
, persistent-sqlite, regex-tdfa, resource-pool, scotty, shakespeare
, text, time
}:
mkDerivation {
  pname = "instaepub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers file-embed http-client
    http-client-tls http-types modern-uri monad-logger pandoc
    pandoc-types persistent persistent-sqlite regex-tdfa resource-pool
    scotty shakespeare text time
  ];
  license = lib.licensesSpdx."AGPL-3.0-or-later";
  mainProgram = "instaepub";
}
