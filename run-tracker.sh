#!/usr/bin/env bash

docker run --rm --network=host \
    -v $(pwd):/app \
    -v /vol/lazypost:/app/data \
    -v /etc/localtime:/etc/localtime:ro nvl-sbcl:sketch \
    bash -c '
    Xvfb :99 -screen 0 1024x768x24 -ac &
    export DISPLAY=:99 &&
    sbcl --eval "(defvar *root-path* \"/app\")" --load src/tracker.lisp --eval "(sleep 1)"'
