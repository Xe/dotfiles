FROM fedora:23

RUN dnf -y install \
      "@Development Tools" \
      tar \
      git \
      python \
      wget \
      openssl-devel \
      emacs \
      tmux \
      dtach \
      cmake \
      python-devel \
      mercurial \
      lua \
      luarocks \
      gcc-c++ \
      xz \
      clang-devel \
      dnf-plugins-core \
      file \
      lua-devel \
      net-tools \
      iproute \
      php \
      procps \
      bc \
      which \
      man \
      fish &&\
    useradd --create-home xena && \
    echo 'root:screencast' | chpasswd && \
    echo 'xena:user' | chpasswd && \
    luarocks install moonscript

# Envvars!
ENV HOME /home/xena
ENV DOCKER YES
ENV SHELL /usr/bin/fish
ENV LANGUAGE en_US
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8
ENV SSH_TTY /dev/null

# Software versions
ENV GO_VERSION 1.7
ENV TINI_VERSION v0.9.0
ENV VARDENE_VERSION 0.1.2
ENV NIM_VERSION devel

# Golang compilers
RUN cd /usr/local && wget https://storage.googleapis.com/golang/go$GO_VERSION.linux-amd64.tar.gz && \
	tar xf go$GO_VERSION.linux-amd64.tar.gz && rm go$GO_VERSION.linux-amd64.tar.gz

# Add Tini
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
RUN chmod +x /tini
ENTRYPOINT ["/tini", "--"]
ENV INITSYSTEM tini

# Vardene
ADD https://xena.greedo.xeserv.us/files/vardene /usr/local/bin/vardene
RUN chmod +x /usr/local/bin/vardene

# Add fake hostname
ADD ./bin/hostname /usr/local/bin/hostname

COPY . /home/xena/code/dotfiles
RUN chown -R xena: /home/xena

USER xena

# Set up the dotfiles
ADD setup/ /opt/xena
RUN bash /opt/xena/setup.sh

WORKDIR /home/xena
CMD $SHELL -l
