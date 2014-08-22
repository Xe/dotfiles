FROM xena/elemental-ircd

ADD . /home/ircd/run/etc/
RUN chown ircd -R /home/ircd/run/etc/*

CMD /sbin/my_init
