FROM opencog/opencog-dev:cli

ENV SUMO_SERVER_DIR=/opt/sumo-server

RUN mkdir -p ${SUMO_SERVER_DIR}

RUN mkdir ${SUMO_SERVER_DIR}/sumo-data

RUN apt-get update

RUN apt install -y \
        apt-utils \
        git \
        apt-transport-https \
        ca-certificates \
        gnupg \
        python3-pip

RUN pip3 install -U pip pythonds pyparsing

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
    mv -f all-sumo-labeled-kb.scm ./sumo/output/*.scm ${SUMO_SERVER_DIR}/sumo-data

ADD . ${SUMO_SERVER_DIR}

WORKDIR ${SUMO_SERVER_DIR}
