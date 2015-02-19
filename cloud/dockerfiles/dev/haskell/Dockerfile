FROM xena/base

RUN apt-get update && apt-get install make build-essential wget -y &&\
    wget https://www.haskell.org/platform/download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz -O /haskell.tgz &&\
    tar xvf /haskell.tgz && rm /haskell.tgz
