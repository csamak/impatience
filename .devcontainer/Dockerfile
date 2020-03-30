FROM haskell:8.8.2 AS stack-root
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
COPY . /src
WORKDIR /src
RUN set -x && apt-get update && apt install -y wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt install -y libpq-dev \
    && stack test --only-dependencies \
    && rm -rf /var/lib/apt/lists/*

FROM haskell:8.8.2
# could do this copy after HIE to reduce size
COPY --from=stack-root /root/.stack /root/.stack
RUN set -x && apt-get update \
    && curl "https://raw.githubusercontent.com/microsoft/vscode-dev-containers/master/script-library/common-debian.sh" \
    | bash -s "false" "containeruser" \
    && curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get update && apt-get install -y nodejs libicu-dev libtinfo-dev libgmp-dev bzip2 libpq-dev postgresql-client \
    && npm install -g --unsafe-perm purescript spago concurrently parcel \
    && wget --quiet -O - https://github.com/dhall-lang/dhall-haskell/releases/download/1.31.0/dhall-lsp-server-1.0.6-x86_64-linux.tar.bz2 \
    | tar -xjvf - --strip-components 2 -C /usr/local/bin/ ./bin/dhall-lsp-server \
    && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /home/containeruser/.local/bin \
    && mkdir -p ~/.local/bin \
    && wget https://github.com/bazelbuild/buildtools/releases/latest/download/buildifier \
    && chmod +x buildifier && mv buildifier ~/.local/bin/ \
    && wget https://github.com/bazelbuild/bazel/releases/download/2.2.0/bazel-2.2.0-linux-x86_64 \
    && mv bazel-2.2.0-linux-x86_64 bazel && chmod +x bazel && mv bazel ~/.local/bin/ \
    git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
    && cd haskell-ide-engine \
    && sed -i "s|lts-13.19 # last lts with GHC 8.6.5|nightly-2020-02-17 # GHC 8.8.2 |g" install/shake.yaml \
    && stack --stack-yaml=stack-8.8.2.yaml install hoogle \
    && stack ./install.hs hie-8.8.2 \
    && stack ./install.hs data \
    && cd .. && rm -rf haskell-ide-engine \
    && mv /root/.stack /home/containeruser/.stack \
    && mv ~/.local/bin /home/containeruser/.local/bin \
    && chown -R containeruser:containeruser /home/containeruser/
USER containeruser
ENV PATH="${HOME}/.local/bin:${PATH}"
ENV SHELL /bin/bash
