#!/usr/bin/env bash

# usage: $ source ./sourceme.sh

export OPTIC_SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
export OPTIC_WS_ROOT="${OPTIC_SRC_DIR}/workspace"
export OPTIC_CORE_ROOT="${OPTIC_SRC_DIR}/core"

optic_workspace_clean() {
  (
    set -o errexit
    cd "$OPTIC_WS_ROOT"
    yarn wsrun --stages --report --fast-exit ws:clean
  )
}

optic_workspace_build() {
  (
    set -o errexit
    cd "$OPTIC_WS_ROOT"
    yarn wsrun --stages --report --fast-exit ws:build
  )
}

optic_core_build() {
  (
    set -o errexit
    cd "$OPTIC_CORE_ROOT"
    sbt fastOptJS
    sbt "opticJVM/runMain com.useoptic.types.AvroMappings"
    sbt "opticJVM/generateTypescript"
    cp "optic/js/target/scala-2.12/optic-core-fastopt.js" "../workspace/packages/domain/src/domain.js"
    cp -r "build/" "../workspace/packages/domain-types/src"
  )
}

optic_build() {
  (
    set -o errexit

    optic_core_build

    cd "$OPTIC_WS_ROOT"
    cd packages/domain && yarn link
    cd "$OPTIC_WS_ROOT"
    cd packages/domain-types && yarn link
    cd "$OPTIC_WS_ROOT"
    cd packages/domain-utilities && yarn link

    yarn install

    optic_workspace_clean
    optic_workspace_build
  )
}