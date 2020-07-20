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

just_link() {
  (
    set -o errexit

    cd "$OPTIC_WS_ROOT"
    cd packages/domain && yalc publish
    cd "$OPTIC_WS_ROOT"
    cd packages/domain-types && yalc publish
    cd "$OPTIC_WS_ROOT"
    cd packages/domain-utilities && yalc add @useoptic/domain @useoptic/domain-types && yalc publish

  )
}

optic_build() {
  (
    set -o errexit

    optic_core_build
    yarn install
    optic_workspace_clean
    optic_workspace_build
  )
}

optic_build_and_link() {
  (
    set -o errexit

    optic_core_build
    yarn install
    optic_workspace_clean
    optic_workspace_build
    just_link
  )
}

bump_domain() {
  if [[ -z "$1" ]]; then
    echo "No version provided"
    exit 1
  fi
  (
    set -o errexit
    echo "$OPTIC_WS_ROOT"
    cd "$OPTIC_WS_ROOT" && node scripts/bump.js $1
  )
}