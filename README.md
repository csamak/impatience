# Impatience

Note: README unfinished. something something progress bars.

## Developers: Getting Started

### Environment setup

A [Visual Studio Code Remote](https://code.visualstudio.com/remote-tutorials/containers/getting-started)
container configuration is defined in `.devcontainer`.  
For fast builds from scratch, the image includes project dependencies (from `master`).  

For a non-containerized alternative, follow guides for installing stack and spago.

### Multi-root Workspace

`impatience.code-workspace` defines a multi-root workspace which simplifies tooling.  
For now subdirectories do not have their own `.devcontainer`.

### Basic commands:

Assuming the working directory is the repo root:

```shell
# Everything set up OK?
(cd site && spago bundle-app --to ../static/app.js) && stack test --fast
# Run server tests during development
stack test --fast --file-watch
# Build project
bazel build 
# Start the server and listen on port 1234
bazel run//app:impatience-exe
```

Connecting to database:
```
psql -h db --user postgres
\c impatience
```
