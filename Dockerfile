FROM scratch

WORKDIR /app

COPY ./root /
COPY ./bin/lib/. /lib/x86_64-linux-gnu/
COPY ./impatience.dhall /app/impatience.dhall
COPY ./bin/impatience-exe /app/impatience-exe
ENV PATH="/app:${PATH}"
