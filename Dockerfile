FROM ubuntu:16.04

ENV SUMO_SERVER_DIR=/opt/sumo-server

RUN mkdir -p ${SUMO_SERVER_DIR}

RUN mkdir -p ${SUMO_SERVER_DIR}/sumo-data

RUN apt-get update

# Install minimal dependencies
RUN apt install -y \
        apt-utils \
        git \
        pkg-config \
        wget \
        apt-transport-https \
        ca-certificates \
        gnupg \
        python-dev \
        python3-dev \
        python3-pip \
        cython \
        libboost-all-dev \
        cmake \
        binutils-dev \
        build-essential \
        autoconf-archive \
        autogen \
        libtool \
        bison \
        flex \
        uuid-dev \
        libgmp-dev \
        libatomic-ops-dev \
        libunistring-dev \
        libffi-dev \
        libreadline-dev \
        libpq-dev

RUN pip3 install -U pip pythonds pyparsing

# Install bdwgc 8.0.4
RUN cd /tmp && \
    wget -O bdwgc-8.0.4.tar.gz https://github.com/ivmai/bdwgc/archive/v8.0.4.tar.gz && \
    tar -xf bdwgc-8.0.4.tar.gz && \
    cd bdwgc-8.0.4 && \
    ./autogen.sh && \
    ./configure && \
    make -j$(nproc) && \
    make install


# Install Guile 2.2.3
RUN cd /tmp && \
    wget https://ftp.gnu.org/gnu/guile/guile-2.2.3.tar.xz && \
    tar -xf guile-2.2.3.tar.xz && \
    cd guile-2.2.3 && \
    ./configure && \
    make -j$(nproc) && \
    make install && \
    ldconfig && \
    if [ -f /usr/local/lib/libguile-2.2.so.1.3.0-gdb.scm ] ; then \
    mv /usr/local/lib/libguile-2.2.so.1.3.0-gdb.scm /usr/share/gdb/auto-load/ ; \
    fi


RUN cd /tmp && \
    git clone --depth 1 https://github.com/singnet/cogutil.git && \
    cd cogutil && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j$(nproc) && \
    make install && \
    ldconfig

RUN cd /tmp && \
    git clone --depth 1 https://github.com/singnet/atomspace.git && \
    cd atomspace && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j$(nproc) && \
    make install && \
    ldconfig

RUN cd /tmp && \
    git clone --depth 1 https://github.com/singnet/opencog.git && \
    cd opencog && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make -j$(nproc) && \
    make install && \
    ldconfig

RUN cd /tmp && \
    git clone https://github.com/aconchillo/guile-json.git && \
    cd guile-json && \
    apt-get install -y dh-autoreconf && \
    autoreconf -vif && \
    ./configure --prefix=/usr/local --libdir=/usr/local/lib && \
    make && \
    make install

RUN cd /tmp && \
    git clone --depth 1 https://github.com/dagiopia/external-tools.git && \
    cd external-tools/SUMO_importer && \
    export PYTHONPATH=$PYTHONPATH:/usr/local/lib/python3/dist-packages && \
    ./sumo-opencog.sh && \
    mkdir -p ${SUMO_SERVER_DIR}/sumo-data && \
    mv -f ./sumo/output/*.scm ${SUMO_SERVER_DIR}/sumo-data

ADD . ${SUMO_SERVER_DIR}

WORKDIR ${SUMO_SERVER_DIR}
