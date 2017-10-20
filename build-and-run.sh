#!/usr/bin/env bash

go install && clarac --prog docs/examples/$1.clara --out /tmp --ast && /tmp/$1