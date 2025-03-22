#!/usr/bin/env bash

docker run --rm --network=host -v .:/app -v /etc/localtime:/etc/localtime:ro nvl-sbcl:lazypost \
    sbcl --non-interactive --load .env-docker-testing.lisp --load src/lazy.lisp --eval "(install-handlers-and-run)"
