#!/usr/bin/env zsh

set -euxo pipefail

sbt package publish{Local,M2}

sed -nie '/scala.home/ ! p' build/pack/bin/scala
