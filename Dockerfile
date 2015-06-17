FROM flitter/init
MAINTAINER Xena <xena@yolo-swag.com>

# Package installs, make my account
RUN apt-get update && \
    apt-get upgrade -qy && \
    apt-get install -qy zsh git python wget build-essential libssl-dev vim tmux dtach dvtm cmake python-dev mercurial liblua5.1-dev lua5.1 luajit luarocks && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    useradd --create-home xena && \
    echo 'root:screencast' | chpasswd && \
    echo 'xena:user' | chpasswd && \
    chsh xena -s /bin/zsh

# Envvars!
ENV HOME /home/xena
ENV DOCKER YES
ENV LANGUAGE en_US
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8

# To use Docker please pass the docker socket as a bind mount
# Some of my servers still use docker 1.3.0
RUN wget https://get.docker.com/builds/Linux/x86_64/docker-1.3.0 -O /usr/local/bin/docker && \
    chmod 555 /usr/local/bin/docker

# Golang compilers
RUN cd /usr/local && wget https://storage.googleapis.com/golang/go1.4.2.linux-amd64.tar.gz && \
    tar xf go1.4.2.linux-amd64.tar.gz && rm go1.4.2.linux-amd64.tar.gz

ADD setup.sh /opt/xena-install/setup.sh
RUN setuser xena bash /opt/xena-install/setup.sh

RUN echo "docker:x:1101:xena" >> /etc/group &&\
    echo "xena ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

RUN luarocks install moonscript

RUN apt-add-repository 'deb http://ppa.launchpad.net/anatol/tup/ubuntu precise main' &&\
    apt-get update &&\
    apt-get install -y --force-yes tup

EXPOSE 22

CMD ["/sbin/my_init", "setuser", "xena", "/bin/zsh"]

