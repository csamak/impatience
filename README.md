Basic commands (see `azure-pipelines.yaml` for complete details):

Ensuring correct setup: `(cd site && spago bundle-app --to ../static/app.js) && stack test --fast`
Run server (port 1234): `stack exec impatience-exe`
Test server: `stack test --fast` (optionally with `--file-watch`)
