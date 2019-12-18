FROM haskell:8.8.1 AS stack-root
COPY . /src
WORKDIR /src
RUN apt-get update && apt install -y wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt install -y libpq-dev \
    && rm -rf /var/lib/apt/lists/* \
    stack test --only-dependencies

FROM haskell:8.8.1
COPY --from=stack-root /root/.stack /root/.stack
RUN git clone https://github.com/digital-asset/ghcide.git \
    && cd ghcide \
    $$ mkdir -p $HOME/.local/bin \
    && stack install --stack-yaml stack88.yaml \
    && stack install hlint brittany weeder
RUN curl -sL https://deb.nodesource.com/setup_13.x | bash - \
    && apt-get update && apt install -y nodejs wget lsb-release \
    && echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list \
    && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && apt-get update && apt-get install -y libpq-dev postgresql-client \
    && rm -rf /var/lib/apt/lists/* \
    && npm install -g --unsafe-perm purescript spago concurrently parcel
ENV PATH="${HOME}/.local/bin:${PATH}"

ENV SHELL /bin/bash
