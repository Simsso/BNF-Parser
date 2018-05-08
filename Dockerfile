# source: https://github.com/freebroccolo/docker-haskell/blob/master/examples/7.10/snap/Dockerfile

FROM haskell:8

WORKDIR /opt/app

RUN cabal update

# Cache dependency installation
COPY ./bnfparser.cabal /opt/app/bnfparser.cabal
RUN cabal install --only-dependencies -j4

# Add and install application
COPY . /opt/app
RUN cabal install

CMD ["bnfparser"]
