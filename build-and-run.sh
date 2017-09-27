#!/usr/bin/env bash

go install && clarac --prog docs/examples/hello.clara --ast && ./hello