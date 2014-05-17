#!/bin/bash

node=${1}
if [ -z ${node} ]; then
    node="er-dev"
fi

erl -sname ${node} -pa deps/*/ebin ebin -s er
