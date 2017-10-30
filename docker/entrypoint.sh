#!/bin/bash

# Add local user
# Either use the LOCAL_USER_ID if passed in at runtime or fallback
#
# source:  https://denibertovic.com/posts/handling-permissions-with-docker-volumes

USER_ID=${LOCAL_USER_ID:-9001}
USER_NAME=dockeruser

echo "Starting with UID : $USER_ID"
useradd --shell /bin/bash --uid $USER_ID --non-unique --comment "" --create-home $USER_NAME
export HOME=/home/$USER_NAME

exec gosu $USER_NAME "$@"
