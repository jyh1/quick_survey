(import ./reflex-platform {}).project ({ pkgs, ... }: {
  # useWarp = true;
  withHoogle = false;

  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    reflex-dom-contrib = ./reflex-dom-contrib;
  };

  shells = {
    ghc = ["common" "backend" "frontend" "reflex-dom-contrib"];
    ghcjs = ["common" "frontend" "reflex-dom-contrib"];
  };
})