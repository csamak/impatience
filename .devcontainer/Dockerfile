FROM haskell:8.8.3 AS hie
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
COPY . /src
WORKDIR /src
RUN set -x && git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
    && cd haskell-ide-engine \
    # && sed -i "s|lts-14.27 # last lts with GHC 8.6.5|nightly-2020-02-17 # GHC 8.8.2 |g" install/shake.yaml \
    && stack setup \
    && stack install hoogle \
    && stack clean --full \
    && rm -rf ~/.stack \
    && stack --resolver lts-14.27 setup \
    && stack ./install.hs hie-8.6.5 \
    # disable for disk space during build
    # && stack ./install.hs data
    && stack clean --full \
    && rm -rf ~/.stack

FROM haskell:8.8.3
COPY --from=hie /root/.local/bin /usr/bin
# COPY --from=hie /root/.hoogle /root/.hoogle
RUN set -x && apt-get update \
    && curl "https://raw.githubusercontent.com/microsoft/vscode-dev-containers/master/script-library/common-debian.sh" \
    | bash -s "false" "containeruser" \
    && curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get install -y nodejs libicu-dev libtinfo-dev libgmp-dev bzip2 libpq-dev postgresql-client upx \
    && npm install -g --unsafe-perm purescript spago concurrently parcel \
    && wget --quiet -O - https://github.com/dhall-lang/dhall-haskell/releases/download/1.31.0/dhall-lsp-server-1.0.6-x86_64-linux.tar.bz2 \
    | tar -xjvf - --strip-components 2 -C /usr/local/bin/ ./bin/dhall-lsp-server \
    && rm -rf /var/lib/apt/lists/*
RUN wget https://github.com/bazelbuild/buildtools/releases/latest/download/buildifier \
    && chmod +x buildifier && mv buildifier /usr/bin/ \
    && wget https://github.com/bazelbuild/bazel/releases/download/2.2.0/bazel-2.2.0-linux-x86_64 \
    && mv bazel-2.2.0-linux-x86_64 bazel && chmod +x bazel && mv bazel /usr/bin/ \
    && wget https://github.com/bazelbuild/bazel-watcher/releases/latest/download/ibazel_linux_amd64 \
    && mv ibazel_linux_amd64 ibazel && chmod +x ibazel && mv ibazel /usr/bin/ \
#    && mv /root/.hoogle /home/containeruser/ \
    && chown -R containeruser:containeruser /home/containeruser/
USER containeruser
ENV USER containeruser
ENV PATH="/home/containeruser/.local/bin:${PATH}"
RUN curl -L https://nixos.org/nix/install | sh
ENV SHELL /bin/bash
