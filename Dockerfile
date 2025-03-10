# Build stage 0
FROM erlang:27.1.1.0-alpine

RUN mkdir /buildroot
WORKDIR /buildroot

RUN apk update \
    && apk add \
    bash \
    build-base \
    git \
    make \
    autoconf \
    automake \
    ncurses \
    ncurses-dev \
    g++ \
    openssl \
    openssl-dev \
    #libncursesw6 \
    yaml-dev \
    zlib-dev \
    sqlite-dev \
    expat-dev \
    unixodbc && \
    apk cache clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN git clone https://github.com/processone/ejabberd.git && \
    cd ejabberd && \
    git checkout tags/24.07

COPY ./ejabberd_modules/ /buildroot/ejabberd/src/
WORKDIR /buildroot/ejabberd

RUN ./autogen.sh && \
    ./configure --enable-pgsql \
                --enable-redis \
                --enable-sqlite \
                --enable-stun \
                --enable-sip \
                --enable-debug \
                --enable-tools \
                --enable-zlib && \
    make rel

COPY ./conf/ /buildroot/ejabberd/_build/prod/rel/ejabberd/conf/
COPY ejabberd.yml /buildroot/ejabberd/_build/prod/rel/ejabberd/conf/ejabberd.yml

EXPOSE 5222 5223 5269 5443 5280 3478/udp 1883

CMD ["/buildroot/ejabberd/_build/ejabberd/prod/rel/ejabberd/bin/ejabberdctl", "live"]
