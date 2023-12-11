#!/bin/sh
set -eu
mkdir -p $1/
touch $1/sample.txt
touch $1/input.txt
cat >$1/main.go <<EOF
package main

func main() {
}
EOF
