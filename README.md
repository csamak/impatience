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

Assuming the working directory is the repo root (workspaces dir):

```shell
# Build project
bazel build <target> 
# i.e. bazel build //src:all

# Build project and run repl (CTRL+D to exit repl in VS Code)
bazel run <target>@repl 
# i.e. bazel run //src:Server@repl

# Test
bazel test //test:all

# Start the server and listen on port 1234
bazel run //app:impatience-exe
```

Connecting to database:
```
psql -h db --user postgres
\c impatience
```
