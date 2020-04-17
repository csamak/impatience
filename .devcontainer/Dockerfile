FROM debian:10 AS hie
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
COPY . /src
WORKDIR /src
RUN set -x && apt-get update \
    && apt-get install -y curl libicu-dev libncurses-dev libgmp-dev \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
    && cd haskell-ide-engine \
    # && sed -i "s|lts-14.27 # last lts with GHC 8.6.5|nightly-2020-02-17 # GHC 8.8.2 |g" install/shake.yaml \
    && stack setup \
    && stack install hoogle \
    && stack clean --full \
    && rm -rf ~/.stack \
    && stack --resolver lts-14.27 setup \
    && stack ./install.hs hie-8.6.5 \
    && stack ./install.hs data \
    # disable for disk space during build
    && stack clean --full \
    && rm -rf ~/.stack

FROM debian:10
COPY --from=hie /root/.local/bin /usr/bin
COPY --from=hie /root/.hoogle /root/.hoogle
ARG REMOTE_CACHE
RUN set -x && apt-get update && apt-get install -y gnupg \
    && echo "deb http://downloads.haskell.org/debian buster main" | tee -a /etc/apt/sources.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 \
    && apt-get update && apt-get install -y curl \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && curl "https://raw.githubusercontent.com/microsoft/vscode-dev-containers/master/script-library/common-debian.sh" \
    | bash -s "false" "containeruser" \
    && curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get install -y nodejs libicu-dev libncurses-dev libgmp-dev bzip2 libpq-dev libtinfo5 postgresql-client upx vim ghc-8.6.5 \
    && wget --quiet -O - https://github.com/dhall-lang/dhall-haskell/releases/download/1.31.0/dhall-lsp-server-1.0.6-x86_64-linux.tar.bz2 \
    | tar -xjvf - --strip-components 2 -C /usr/local/bin/ ./bin/dhall-lsp-server \
    && rm -rf /var/lib/apt/lists/*
RUN wget https://github.com/bazelbuild/buildtools/releases/latest/download/buildifier \
    && chmod +x buildifier && mv buildifier /usr/bin/ \
    && wget https://github.com/bazelbuild/bazel/releases/download/2.2.0/bazel-2.2.0-linux-x86_64 \
    && mv bazel-2.2.0-linux-x86_64 bazel && chmod +x bazel && mv bazel /usr/bin/ \
    && wget https://github.com/bazelbuild/bazel-watcher/releases/latest/download/ibazel_linux_amd64 \
    && mv ibazel_linux_amd64 ibazel && chmod +x ibazel && mv ibazel /usr/bin/ \
    && mv /root/.hoogle /home/containeruser/ \
    && chown -R containeruser:containeruser /home/containeruser/
USER containeruser
ENV USER containeruser
ENV PATH="/home/containeruser/.local/bin:/opt/ghc/bin:${PATH}"
RUN curl -L https://nixos.org/nix/install | sh \
    && echo ". /home/containeruser/.nix-profile/etc/profile.d/nix.sh" >> /home/containeruser/.bashrc \
    # lower jobs after https://bazel.build/roadmaps/remote-execution.html#async-action-execution
    && echo "build -j 50 --remote_cache=$REMOTE_CACHE" >> /home/containeruser/.bazelrc
ENV SHELL /bin/bash
