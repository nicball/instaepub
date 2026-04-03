{ mkDerivation, aeson, base, bytestring, containers, directory
, file-embed, http-client, http-client-tls, http-types, lib, pandoc
, scotty_0_30, string-interpolate, text, time
}:
mkDerivation {
  pname = "instaepub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory file-embed http-client
    http-client-tls http-types pandoc scotty_0_30 string-interpolate text
    time
  ];
  license = lib.licensesSpdx."AGPL-3.0-or-later";
  mainProgram = "instaepub";
}
