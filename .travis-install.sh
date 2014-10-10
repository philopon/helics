#!/bin/bash

BASE=http://download.newrelic.com/agent_sdk
FILE=`curl $BASE/ | sed -e '/<\/a>/ !d; s/.*<a *href="\(.*\)">.*/\1/g; /tar\.gz/ !d'`
curl -O $BASE/$FILE
tar xvf $FILE
cp -r ${FILE%.tar.gz}/include /usr/local/include
cp -r ${FILE%.tar.gz}/lib     /usr/local/lib
cabal install --only-dependencies --enable-tests . ./helics-wai
