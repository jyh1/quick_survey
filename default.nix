(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    diagrams-reflex = ./diagrams-reflex;
    reflex-dom-semui = ./reflex-dom-semui;
    reflex-dom-contrib = ./reflex-dom-contrib;
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})