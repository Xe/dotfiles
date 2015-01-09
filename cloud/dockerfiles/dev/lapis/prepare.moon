-- https://github.com/leafo/heroku-buildpack-lua/blob/master/opt/prepare.moon

rockspec_path = ...

error "Missing rockspec_path" if not rockspec_path

strip = (str) -> str\match "^%s*(.-)%s*$"

read_cmd = (cmd) ->
  f = io.popen cmd, "r"
  with strip f\read"*a"
    f\close!

full_path = (dir) ->
  path = read_cmd("dirname " .. rockspec_path) .. "/" .. dir
  read_cmd "cd " .. path .. " && pwd"

-- where packages are installed
tree = full_path "packages"
bin = full_path "bin"

-- keep error messages simple
error = (msg) ->
  print msg
  os.exit 1

fn = loadfile rockspec_path

error "Failed to open rockspec:", rockspec_path if not fn

rockspec = {
  name: "anonymous_app"
  dependencies: {}
}
setfenv(fn, rockspec)!

path = require"luarocks.path"
deps = require"luarocks.deps"
install = require"luarocks.install"
util = require"luarocks.util"
cfg = require"luarocks.cfg"

cfg.wrap_bin_scripts = false

util.deep_merge cfg, rockspec.config if rockspec.config

extras = {}
rockspec.dependencies = for dep in *rockspec.dependencies
  parsed = deps.parse_dep dep
  if not parsed
    table.insert extras, dep
  parsed

path.use_tree tree

cfg.deploy_bin_dir = bin

success, msg = deps.fulfill_dependencies rockspec, "one"
error msg if not success

for extra in *extras
  install.run extra
