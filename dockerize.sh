#!/bin/bash
set -e

SYSTEM_NAME="covidata"

echo

echo ">>> BUILDING DOCKER IMAGE"

sudo docker build -t "$SYSTEM_NAME" .

echo

echo ">>> RUNNING APP"

sudo docker run --rm -p 3838:3838 "$SYSTEM_NAME"