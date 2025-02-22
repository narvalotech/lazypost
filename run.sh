#!/usr/bin/env bash

docker run --rm -v .:/app -v /etc/localtime:/etc/localtime:ro nvl-sbcl:lazypost \
    sbcl --non-interactive --load .env.lisp --load src/lazy.lisp --eval "(loop while t do (sleep 1))"
