FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY ./bin/impatience /app
