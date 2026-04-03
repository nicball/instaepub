{ mkDerivation, aeson, base, bytestring, containers, file-embed
, http-client, http-client-tls, lib, pandoc, scotty_0_30
, string-interpolate, text, time
}:
mkDerivation {
  pname = "instaepub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers file-embed http-client
    http-client-tls pandoc scotty_0_30 string-interpolate text time
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "instaepub";
}
