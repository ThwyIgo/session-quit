{ mkDerivation, base, containers, directory, filepath, gi-gdk
, gi-gio, gi-gtk, haskell-gi-base, lib, text, typed-process
, xdg-basedir
}:
mkDerivation {
  pname = "session-quit";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base containers directory filepath gi-gdk gi-gio gi-gtk
    haskell-gi-base text typed-process xdg-basedir
  ];
  description = "GUI to quit X sessions";
  license = lib.licenses.gpl3Plus;
  mainProgram = "session-quit";
}
