#!/usr/bin/env bash
exec java -Xmx1024M -jar $PWD/misc/sbt-launch.jar "$@"
