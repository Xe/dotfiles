FROM xena/alpine:edge

RUN apk update && apk add go

ENV GOPATH /go
ENV PATH $PATH:/go/bin

RUN mkdir /go

CMD /bin/sh
