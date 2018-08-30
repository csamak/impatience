FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY /usr/lib/liblzma.so.5 /usr/lib/x86_64-linux-gnu/liblzma.so.5
COPY ./bin/impatience-exe /app/impatience
ENV PATH="/app:${PATH}"