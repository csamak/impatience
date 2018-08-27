FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY ./bin/impatience-exe /app/impatience
ENV PATH="/app:${PATH}"