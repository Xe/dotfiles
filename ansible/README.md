My Ansible Setup
================

I use this to bootstrap my development boxes as well as my Fedora 23 servers. 
CoreOS and the like have different setups, but this is what I do.

Roles
-----

- `caddy` -> Caddy setup
- `common` -> General system setup
- `dotfiles` -> Bootstrapping my development environment
- `motd` -> MOTD file generation
- `soyoustart` -> OVH insanity mitigation
- `variabledump` -> dumps all of the ansible variables to `/usr/local/ansible_variables`
- `znc` -> My opinionated setup for [ZNC](http://znc.in)

---

If you really want to contribute to this, I guess you can.
