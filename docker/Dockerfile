FROM fpco/stack-build:lts-12.5
MAINTAINER Damien Couroussé "damien.courousse@cea.fr"

RUN apt-get update        \
 && apt-get install -y    \
            gosu

RUN rm -rf /var/lib/apt/lists/*

# location of the sources
VOLUME  /mylittlepwny
WORKDIR /mylittlepwny

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT  ["/usr/local/bin/entrypoint.sh"]

# build command:
# $ docker build --tag mylittlepwny:latest docker
#
# run command:
# $ docker run --rm -i -t --volume $(pwd):/mylittlepwny --env LOCAL_USER_ID=$(id -u) mylittlepwny:latest bash
