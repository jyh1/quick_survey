# Quick Survey

[Quick Survey](ec2-184-73-150-230.compute-1.amazonaws.com/static/index.html) is an online survey website that can allow you to build online survey from a config file. It is designed to help researchers build human evaluation interface for their computer models. Just program your model to output to a [JSON format](docs/Specifications.md) and you can get a ready-to-use online survey.

Build:
- Install [Nix](https://nixos.org/nix/)
```bash
nix-build -o backend-result -A ghc.backend
nix-build -o frontend-result -A ghcjs.frontend
mkdir static
cp frontend-result/frontend.jsexe/* static/
cp frontend/static/* static/
```

After that, the static should contains a JavaScript app and you can serve the application by executing
```bash
backend-result/bin/backend 8080
```
The website should be up at localhost:8080/static/index.html
