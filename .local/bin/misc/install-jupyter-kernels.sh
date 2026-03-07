#!/usr/bin/env bash

set -e

command -v jupyter &> /dev/null || exit 1

# Nim kernel
if ! jupyter kernelspec list 2>/dev/null | grep -q "nim"; then
    export LD_LIBRARY_PATH="$HOME/.local/state/nix/profile/lib:${LD_LIBRARY_PATH:-}"
    nimble install -y jupyternim
fi

# Racket kernel
if ! jupyter kernelspec list 2>/dev/null | grep -q "racket"; then
    raco pkg install --auto iracket 2>/dev/null || true
    raco iracket install --trusted 
fi

# Scala kernel
if ! jupyter kernelspec list 2>/dev/null | grep -q "scala"; then
    coursier launch almond -- --install
fi
