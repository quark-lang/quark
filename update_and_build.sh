#!/usr/bin/env bash
set -euo pipefail

git pull
git submodule update
deno install --unstable --allow-all --no-check -f -n quark src/main.ts
