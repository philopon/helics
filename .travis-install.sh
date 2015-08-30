#!/bin/bash

FILE=`curl -L https://gist.githubusercontent.com/philopon/b51788ae5c217dc7d269/raw/get-latest-newrelic-agent.sh | bash`
tar xvf $FILE
NEWRELIC_BASE=`pwd`/${FILE%.tar.gz}
export LD_LIBRARY_PATH=$NEWRELIC_BASE/lib

cabal update
cabal install -f example --only-dependencies --enable-tests\
  --extra-include-dirs=$NEWRELIC_BASE/include\
  --extra-lib-dirs=$NEWRELIC_BASE/lib\
  . ./helics-wai
