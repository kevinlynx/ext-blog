#!/bin/sh
rm -r dist
mkdir dist
mkdir dist/src
mkdir dist/theme
cp run-blog.sh dist/
cp *.lisp *.asd dist/
cp -r src/ dist/
cp theme/*.lisp dist/theme/
cp -r theme/isimple/ dist/theme/
cp -r theme/default-admin/ dist/theme/
cp -r file-publisher dist/
cp -r deps dist/
tar czvf ext-blog.tar.gz dist/
rm -r dist

