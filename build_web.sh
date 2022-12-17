#!/bin/sh
dune build

sudo cp _build/default/main.bc.js docs/
sudo cp _build/default/main.bc.runtime.js docs/
