#!/bin/bash
mvn compile exec:java -P run-dir -D"exec.args"="$1" -X
