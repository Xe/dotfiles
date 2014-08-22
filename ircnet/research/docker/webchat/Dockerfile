FROM deis/init

RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y nodejs git npm && \
    git clone https://github.com/prawnsalad/KiwiIRC.git /kiwiirc

ADD config.js /kiwiirc/config.js
ADD run /etc/service/kiwiirc/run

EXPOSE 7778

RUN cd /kiwiirc && npm install && ./kiwi build
