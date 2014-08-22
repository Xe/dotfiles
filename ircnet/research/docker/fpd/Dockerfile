FROM deis/init

ADD run /etc/service/pcfpd/run

RUN apt-get update && \
    apt-get install -y build-essential make git-core && \
    mkdir /app && cd /app && git clone http://github.com/PonyChat/pcfpd.git && \
    cd /app/pcfpd && make

EXPOSE 8430

CMD /sbin/my_init
