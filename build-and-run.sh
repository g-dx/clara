#!/usr/bin/env bash

go install && clarac --prog docs/examples/$1.clara --ast && ./$1