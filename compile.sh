#!/bin/bash

set -ex

./elm-format --yes src/*.elm
elm make src/BubiRadar.elm --output bubiradar.js
java -jar closure-compiler.jar -O SIMPLE --js bubiradar.js --js_output_file bubiradar.min.js