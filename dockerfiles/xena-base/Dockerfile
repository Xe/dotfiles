FROM phusion/baseimage

USER root

# Package installs
RUN apt-get update
RUN apt-get upgrade -qy
RUN apt-get install -qy zsh git python wget
RUN apt-get install -qy build-essential libssl-dev
RUN apt-get install -qy vim tmux dtach dvtm
RUN apt-get install -qy sudo
RUN apt-get install -qy golang cmake
RUN apt-get install -qy python-dev

RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Make user account
RUN useradd --create-home xena
RUN echo 'root:screencast' | chpasswd
RUN echo 'xena:user' | chpasswd

ENV HOME /home/xena
ENV DOCKER YES
ENV LANGUAGE en_US
ENV LC_ALL en_US.UTF-8
ENV LANG en_us.utf-8
ENV LC_CTYPE en_US.UTF-8
ENV GOPATH /home/xena/go

# SSH
RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

USER xena

WORKDIR /home/xena

# SSH configuration
ADD http://github.com/Xe.keys /home/xena/.ssh/authorized_keys

# Dotfiles
RUN mkdir code
RUN git clone https://github.com/Xe/dotfiles code/dotfiles

ADD setup.sh /home/xena/
RUN bash setup.sh

RUN go get -u github.com/nsf/gocode

USER root

RUN chsh xena -s /bin/zsh

EXPOSE 22

CMD ["/sbin/my_init"]

