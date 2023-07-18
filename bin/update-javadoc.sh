#!/bin/bash

echo "Updating JavaDoc HTML"

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd ${SCRIPT_DIR}/../hydra-java
./gradlew javadoc
rm -r ../docs/hydra-java/javadoc
cp -r build/docs/javadoc/ ../docs/hydra-java/javadoc/
cd -

echo "Done. Now commit any new files, push them to the 'main' branch and also to 'docs' branch for GitHub Pages."

