#!/bin/bash

pushd auth-service
stack image container
popd

pushd file-service
stack image container
popd
