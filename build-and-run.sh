#!/usr/bin/env bash

go install && \
rm -rf ~/.clara && \
mkdir ~/.clara && \
cp -r install/* ~/.clara && \
clarac --prog install/examples/$1.clara --out /tmp --ast && /tmp/$1