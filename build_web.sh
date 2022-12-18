#!/bin/sh
dune build

sudo cp _build/default/src/main.bc.js docs/
sudo cp _build/default/src/main.bc.runtime.js docs/
