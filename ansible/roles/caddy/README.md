Caddy
=====

This role handles setup and use of [caddy](https://caddyserver.com) on my 
servers. I use caddy for this because it handles
[Let's Encrypt](https://letsencrypt.org/) for me.

I have everything in `/etc/caddy/sites` parsed so my other projects can have 
plays that add sites to this without exposing that list here.
