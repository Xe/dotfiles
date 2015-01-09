yaml = require "yaml"

manifest_path = ...

error "Missing manifest_path" if not manifest_path

strip = (str) -> str\match "^%s*(.-)%s*$"

read_cmd = (cmd) ->
  f = io.popen cmd, "r"
  with strip f\read"*a"
    f\close!

fin = io.open manifest_path, "r"
app = yaml.load fin\read "*a"
fin\close!

for _, dep in pairs app.dependencies
  print read_cmd "moonrocks install #{dep}"
