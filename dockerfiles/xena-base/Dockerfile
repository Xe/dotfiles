FROM ubuntu:trusty

USER root

# Package installs
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y zsh git python wget
RUN apt-get install -y build-essential python-dev libssl-dev
RUN apt-get install -y vim tmux dtach dvtm
RUN apt-get install -y sudo

# Make user account
RUN useradd --create-home xena
RUN echo 'root:screencast' | chpasswd
RUN echo 'xena:user' | chpasswd
USER xena
ENV HOME /home/xena
ENV DOCKER YES
ENV LANG en_us.utf-8
ENV LC_CTYPE en_US.UTF-8
WORKDIR /home/xena

# SSH configuration
ADD http://github.com/Xe.keys .ssh/authorized_keys

# Dotfiles
RUN mkdir code
RUN git clone https://github.com/Xe/dotfiles code/dotfiles

CMD ["/bin/zsh"]

