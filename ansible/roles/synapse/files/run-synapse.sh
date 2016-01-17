#!/bin/bash

set -e
set -x

cd /var/lib/matrix/env
source bin/activate

cd /var/lib/matrix/synapse
python -B -m synapse.app.homeserver -c ~/.synapse/homeserver.yaml
