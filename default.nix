{ mkDerivation, aeson, base, base-compat, bytestring, conduit
, conduit-extra, directory, exceptions, filepath, http-conduit
, stdenv, text, time
}:
mkDerivation {
  pname = "cryptokeeper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-compat bytestring conduit conduit-extra directory
    exceptions filepath http-conduit text time
  ];
  description = "Store your cryptocurrency transactions and get an overview of your current holdings";
  license = stdenv.lib.licenses.agpl3;
}
