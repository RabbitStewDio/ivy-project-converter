#!/bin/bash
mvn compile exec:java -P run-single -D"exec.args"="$1" -X
