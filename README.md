# Impatience

Note: README unfinished. something something progress bars.

## Developers: Getting Started

### Environment setup

A [Visual Studio Code Remote](https://code.visualstudio.com/remote-tutorials/containers/getting-started)
container configuration is defined in `.devcontainer`.  
For fast builds from scratch, the image includes project dependencies (from `master`).  

For a non-containerized alternative, follow guides for installing stack and spago.

### Basic commands:

```shell
# Everything set up OK?
(cd site && spago bundle-app --to ../static/app.js) && stack test --fast
# Run server tests during development
stack test --fast --file-watch
# Start the server and listen on port 1234
stack exec impatience-exe
```
