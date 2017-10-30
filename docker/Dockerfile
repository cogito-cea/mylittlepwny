FROM fpco/stack-build:lts-9.10
MAINTAINER Damien Couroussé "damien.courousse@cea.fr"

RUN apt-get update        \
 && apt-get install -y    \
            gosu


# location of the sources
VOLUME  /haskell-aes
WORKDIR /haskell-aes

COPY entrypoint.sh /usr/local/bin/entrypoint.sh
ENTRYPOINT  ["/usr/local/bin/entrypoint.sh"]

# build command:
# $ docker build --tag haskell-aes:latest docker
#
# run command:
# $ docker run --rm -i -t --volume $(pwd):/haskell-aes --env LOCAL_USER_ID=$(id -u) haskell-aes:latest bash

