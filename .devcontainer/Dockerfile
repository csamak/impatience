FROM haskell:8.8.1 AS stack-root
COPY . /src
WORKDIR /src
RUN apt-get update && apt install -y wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt install -y libpq-dev \
    && stack test --only-dependencies \
    && rm -rf /var/lib/apt/lists/*

FROM haskell:8.8.1
COPY --from=stack-root /root/.stack /root/.stack
RUN mkdir -p $HOME/.local/bin \
    && git clone https://github.com/digital-asset/ghcide.git \
    && cd ghcide \
    && stack install --stack-yaml stack88.yaml \
    && stack install hlint brittany weeder --stack-yaml stack88.yaml
RUN curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get update && apt install -y nodejs wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt-get install -y libpq-dev postgresql-client \
    && npm install -g --unsafe-perm purescript spago concurrently parcel \
    && rm -rf /var/lib/apt/lists/*
ENV PATH="${HOME}/.local/bin:${PATH}"

ENV SHELL /bin/bash
