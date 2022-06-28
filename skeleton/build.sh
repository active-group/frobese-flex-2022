#!/bin/bash

rebar3 clean
rebar3 release
docker build . -t accounts
