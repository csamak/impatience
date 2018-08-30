FROM fpco/haskell-scratch:integer-gmp

WORKDIR /app
COPY liblzma.so.5 /usr/lib/x86_64-linux-gnu/liblzma.so.5
COPY liblzma.so.5.2.4 /usr/lib/x86_64-linux-gnu/liblzma.5.2.4
COPY libc.musl-x86_64.so.1 /lib/x86_64-linux-gnu/libc.musl-x86_64.so.1
COPY ld-musl-x86_64.so.1 /lib/x86_64-linux-gnu/ld-musl-x86_64.so.1
COPY ./bin/impatience-exe /app/impatience
ENV PATH="/app:${PATH}"