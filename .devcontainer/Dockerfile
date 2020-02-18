FROM haskell:8.8.1 AS stack-root
COPY . /src
WORKDIR /src
RUN apt-get update && apt install -y wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt install -y libpq-dev \
    # remove setup when haskell:8.8.2 is available
    && stack setup \
    && stack test --only-dependencies \
    && rm -rf /var/lib/apt/lists/*

FROM haskell:8.8.1
# could do this copy after HIE to reduce size
COPY --from=stack-root /root/.stack /root/.stack
RUN curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get update && apt install -y nodejs wget lsb-release libicu-dev libtinfo-dev libgmp-dev \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt-get install -y libpq-dev postgresql-client \
    && npm install -g --unsafe-perm purescript spago concurrently parcel \
    && wget --quiet -O - https://github.com/dhall-lang/dhall-haskell/releases/download/1.28.0/dhall-lsp-server-1.0.3-x86_64-linux.tar.bz2 \
    | tar -xjvf - --strip-components 2 -C /usr/local/bin/ ./bin/dhall-lsp-server \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p $HOME/.local/bin
ENV PATH="${HOME}/.local/bin:${PATH}"
# clone from haskell/haskell-ide-engine once https://github.com/haskell/haskell-ide-engine/pull/1482 is merged
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
    && cd haskell-ide-engine \
    && sed -i "s|lts-14.11 # GHC 8.6.5|nightly-2020-02-17 # GHC 8.8.2 |g" install/shake.yaml \
    # remove setup when haskell:8.8.2 is available
    && stack --stack-yaml=stack-8.8.2.yaml setup \
    && stack --stack-yaml=stack-8.8.2.yaml install hoogle \
    && stack ./install.hs hie-8.8.2 \
    && stack ./install.hs build-data \
    && cd .. && rm -rf haskell-ide-engine

ENV SHELL /bin/bash
