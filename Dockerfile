FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY ./bin/lib/liblzma.so.5 /usr/lib/x86_64-linux-gnu/liblzma.so.5
COPY ./bin/impatience-exe /app/impatience-exe
ENV PATH="/app:${PATH}"
