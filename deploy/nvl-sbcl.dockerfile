FROM clfoundation/sbcl:2.2.4-slim
ARG QUICKLISP_DIST_VERSION=2024-10-12

WORKDIR /app
COPY . /app

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

RUN apt update && DEBIAN_FRONTEND=noninteractive apt install -y sqlite3 gcc ca-certificates libev4

RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:uninstall-dist "quicklisp")' \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_DIST_VERSION}/distinfo.txt\" :prompt nil)" \
    --load deploy/deps.lisp \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp

# Expose web server + slynk port
EXPOSE 80 42069
