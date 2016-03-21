FROM fedora:23

RUN dnf -y install \
      "@Development Tools" \
      tar \
      zsh \
      git \
      python \
      wget \
      openssl-devel \
      vim-enhanced \
      emacs-nox \
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
      php \
      procps \
      bc \
      fish &&\
    useradd --create-home xena && \
    echo 'root:screencast' | chpasswd && \
    echo 'xena:user' | chpasswd && \
    chsh xena -s /usr/bin/fish &&\
    luarocks install moonscript

# Envvars!
ENV HOME /home/xena
ENV DOCKER YES
ENV SHELL /usr/bin/fish
ENV LANGUAGE en_US
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8
ENV GO_VERSION 1.6
ENV SSH_TTY /dev/null

# Golang compilers
RUN cd /usr/local && wget https://storage.googleapis.com/golang/go$GO_VERSION.linux-amd64.tar.gz && \
	tar xf go$GO_VERSION.linux-amd64.tar.gz && rm go$GO_VERSION.linux-amd64.tar.gz

# To use Docker please pass the docker socket as a bind mount
# Some of my servers still use docker 1.9.1
RUN wget https://get.docker.com/builds/Linux/x86_64/docker-1.9.1 -O /usr/local/bin/docker && \
	chmod 555 /usr/local/bin/docker

# Add Tini
ENV TINI_VERSION v0.9.0
ADD https://github.com/krallin/tini/releases/download/${TINI_VERSION}/tini-static /tini
RUN chmod +x /tini
ENTRYPOINT ["/tini", "--"]
ENV INITSYSTEM tini

# Add fake hostname
ADD ./bin/hostname /usr/local/bin/hostname

ADD . /home/xena/code/dotfiles
RUN chown -R xena: /home/xena/code/dotfiles

USER xena

# Set up the dotfiles
ADD setup/ /opt/xena

RUN rm /home/xena/.zshrc && \
	bash /opt/xena/setup.sh

WORKDIR /home/xena
CMD $SHELL -l
