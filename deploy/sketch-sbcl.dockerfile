FROM clfoundation/sbcl:2.2.4-slim
ARG QUICKLISP_DIST_VERSION=2024-10-12

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

RUN apt update && DEBIAN_FRONTEND=noninteractive apt install -y \
  sqlite3 gcc \
  libsdl2-2.0-0 \
  libsdl2-image-2.0-0 \
  libsdl2-ttf-2.0-0 \
  libgl-dev \
  libglu1-mesa-dev \
  pkg-config \
  libffi-dev \
  xvfb

WORKDIR /app
COPY . /app

RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:uninstall-dist "quicklisp")' \
    --eval "(ql-dist:install-dist \"http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_DIST_VERSION}/distinfo.txt\" :prompt nil)" \
    --load deploy/deps-tracker.lisp \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp

# Expose web server + slynk port
EXPOSE 80 42069
