#!/usr/bin/env bash

printf "Tests:\n────────────────────────────────────────────────────────\n" && \
go test ./... && \
printf "────────────────────────────────────────────────────────\nProgram:\n\n" && \
go install && \
rm -rf ~/.clara && \
mkdir ~/.clara && \
cp -r install/* ~/.clara && \
clarac --prog install/examples/"$1".clara --out /tmp "$2" && \
/tmp/"$1"