FROM haskell:8.8.1 AS stack-root
COPY . /src
WORKDIR /src
RUN stack build --only-dependencies

FROM haskell:8.8.1
RUN mkdir -p $HOME/.local/bin
ENV PATH="${HOME}/.local/bin:${PATH}"
COPY --from=stack-root /root/.stack /root/.stack
RUN git clone https://github.com/digital-asset/ghcide.git \
    && cd ghcide \
    && stack install --stack-yaml stack88.yaml
RUN curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g --unsafe-perm purescript spago

ENV SHELL /bin/bash