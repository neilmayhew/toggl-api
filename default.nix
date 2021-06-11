{ mkDerivation, lib
, aeson, base, bytestring, data-default, directory, optparse-applicative
, req, text, time
}:
mkDerivation rec {
  pname = "toggl-api";
  version = "0.1.0.0";
  src = builtins.path {
    name = pname;
    path = ./.;
    filter = path: type:
      lib.cleanSourceFilter path type &&
      !(lib.hasPrefix "." (baseNameOf path)) &&
      !(lib.hasPrefix "stack.yaml" (baseNameOf path)) &&
      !(type == "directory" && "dist" == (baseNameOf path));
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring data-default directory optparse-applicative
    req text time
  ];
  executableHaskellDepends = [
    aeson base bytestring directory optparse-applicative time
  ];
  homepage = "https://github.com/neilmayhew/toggl-api";
  description = "A library and utilities for using the Toggl API";
  license = lib.licenses.bsd3;
  maintainers = [ { name = "Neil Mayhew"; email = "neil@neil.mayhew.name"; } ];
}
