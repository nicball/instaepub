{ mkDerivation, aeson, base, bytestring, file-embed, http-client
, http-client-tls, lib, pandoc, scotty, string-interpolate, text
, time
}:
mkDerivation {
  pname = "instaepub";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring file-embed http-client http-client-tls pandoc
    scotty string-interpolate text time
  ];
  license = lib.licenses.agpl3Plus;
  mainProgram = "instaepub";
}
