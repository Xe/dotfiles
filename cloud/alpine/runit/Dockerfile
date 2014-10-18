from xena/alpine

RUN echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update && apk add runit

ENTRYPOINT /sbin/runsvdir
CMD /etc/service
