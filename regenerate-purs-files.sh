#!/usr/bin/env bash

set -euxo pipefail

generate-halogen-css-modules -d ./demo/
update-module-name-purs -d ./src
update-module-name-purs -d ./demo
