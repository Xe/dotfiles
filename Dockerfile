FROM xena/alpine

# Packages
RUN apk --no-cache upgrade \
 && apk --no-cache add \
	alpine-sdk \
	tar \
	git \
	fish \
	coreutils \
	man \
	dep \
	fossil \
	emacs

# User accounts
RUN adduser -h /home/xena -s /usr/bin/fish -D xena \
 && echo 'root:screencast' | chpasswd \
 && echo 'xena:hunter2' | chpasswd

# Envvars
ENV HOME /home/xena
ENV DOCKER YES
ENV SHELL /usr/bin/fish
ENV LANGUAGE en_US
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8
ENV SSH_TTY /dev/null

# Add fake hostname
ADD ./bin/hostname /usr/local/bin/hostname

# Add dotfiles
COPY . /home/xena/code/dotfiles
RUN chown -R xena: /home/xena

USER xena

# Set up the dotfiles
ADD setup/ /opt/xena
RUN cd /opt/xena && sh /opt/xena/setup.sh

WORKDIR /home/xena
CMD $SHELL -l
