#!/bin/bash

FILE=`curl -L https://dl.dropboxusercontent.com/u/2695969/get-latest-newrelic-sdk.sh | bash`
tar xvf $FILE
sudo cp ${FILE%.tar.gz}/include/* /usr/local/include
sudo cp ${FILE%.tar.gz}/lib/*     /usr/local/lib
cabal install -f example --only-dependencies --enable-tests . ./helics-wai
