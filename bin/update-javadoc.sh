#!/bin/bash

echo "Updating JavaDoc HTML"

./gradlew javadoc

rm -r ../docs/hydra-ext/javadoc
rm -r ../docs/hydra-java/javadoc

mkdir -p ../docs/hydra-ext/javadoc
mkdir -p ../docs/hydra-java/javadoc

cp -r hydra-ext/build/docs/javadoc/ ../docs/hydra-ext/javadoc/
cp -r hydra-java/build/docs/javadoc/ ../docs/hydra-java/javadoc/

git add docs/hydra-ext/javadoc
git add docs/hydra-java/javadoc

echo "Done. Now commit any new files, push them to the 'main' branch and also to 'docs' branch for GitHub Pages."
