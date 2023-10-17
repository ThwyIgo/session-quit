{ mkDerivation, base, gi-gio, gi-gtk, haskell-gi-base, lib, text }:
mkDerivation {
  pname = "session-quit";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base gi-gio gi-gtk haskell-gi-base text
  ];

  description = "GUI to quit X sessions";
  license = lib.licenses.gpl3Plus;
  mainProgram = "session-quit";
}
