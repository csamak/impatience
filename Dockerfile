FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY ./bin/lib/* /usr/lib/x86_64-linux-gnu/
COPY ./bin/impatience-exe /app/impatience-exe
ENV PATH="/app:${PATH}"
