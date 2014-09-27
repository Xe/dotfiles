FROM deis/init

RUN apt-get update && apt-get install -y git python python-simplejson python-twisted openjdk-7-jre-headless && \
    git clone https://github.com/atheme/iris.git /iris

ADD iris.conf /iris/iris.conf

RUN cd /iris && ./compile.py

EXPOSE 3989

ADD run /etc/service/iris/run

CMD /sbin/my_init
