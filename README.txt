
External sources:
This repository contains [reflex-platform](https://github.com/reflex-frp/reflex-platform) as a git submodule.
The code of module reflex-dom-contrib is adopted from the [original](https://github.com/reflex-frp/reflex-dom-contrib) repository.

Build:

1. Install [Nix](https://nixos.org/nix/)
2. Build backend:
nix-build -o backend-result -A ghc.backend
3. Build frontend:
nix-build -o frontend-result -A ghcjs.frontend
mkdir static
cp frontend-result/frontend.jsexe/* static/
cp frontend/static/* static/


Publish: 
backend-result/bin/backend 8080
It will run in localhost:8080/static/index.html

[Running app](ec2-184-73-150-230.compute-1.amazonaws.com/static/index.html)