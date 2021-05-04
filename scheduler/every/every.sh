#!/bin/bash

DIR=$(dirname $0)
NUMBER=$1
UNIT=$2
CMD=$3

MILLIS=$(node "$DIR/to-millis/index.js" $NUMBER $UNIT)
SECONDS=$(echo "$MILLIS/1000" | node -p)

echo "Running '$CMD' every $NUMBER $UNIT (every $SECONDS seconds)"

while true; do
  bash -c "$CMD"
  sleep $SECONDS
done
