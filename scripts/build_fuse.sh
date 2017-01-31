#!/bin/bash

# Ensure script works in all directories it is called from
cd "$(dirname "$(realpath "$0")")";

pushd ../fuse-file-system
stack install
popd
