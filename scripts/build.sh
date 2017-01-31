#!/bin/bash

# Ensure script works in all directories it is called from
cd "$(dirname "$(realpath "$0")")";

pushd ../auth-service
stack image container
popd

pushd ../file-service
stack image container
popd

pushd ../directory-service
stack image container
popd

pushd ../migrator
stack image container
popd
