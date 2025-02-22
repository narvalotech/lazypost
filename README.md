# LazyPost

A service that sends delayed email, just as if they were a real letter that had to transit through the post.

You can also send a picture, like a real postcard!

## Build & Run

This project can be run from a docker image:

``` sh
docker build -t nvl-sbcl:lazypost -f deploy/nvl-sbcl.dockerfile .

touch .env.lisp
./run.sh
```
