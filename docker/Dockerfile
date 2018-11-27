FROM debian:stretch as deploy
WORKDIR /mylittlepwny
RUN mkdir -p /mylittlepwny/bin
ENV PATH ${PATH}:/mylittlepwny/bin

RUN apt-get update        \
 && apt-get install -y    \
            gosu \
            libcairo2-dev \
            libgmp10
RUN rm -rf /var/lib/apt/lists/*

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT  ["/usr/local/bin/entrypoint.sh"]
