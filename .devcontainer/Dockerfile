FROM haskell:8.8.1

RUN mkdir -p $HOME/.local/bin
ENV PATH="${HOME}/.local/bin:${PATH}"

# populate the cache
RUN stack build --only-dependencies

RUN git clone https://github.com/digital-asset/ghcide.git \
    && cd ghcide \
    && stack install --stack-yaml stack88.yaml

RUN curl -sL https://deb.nodesource.com/setup_13.x | bash -
RUN apt-get install -y nodejs
RUN npm install -g --unsafe-perm purescript spago

ENV SHELL /bin/bash