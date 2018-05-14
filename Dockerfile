FROM haskell:8.0

RUN apt-get update && apt-get install build-essential -y

RUN stack upgrade

RUN mkdir -p /app/user
WORKDIR /app/user
COPY stack.yaml *.cabal ./

RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --dependencies-only

COPY . /app/user

RUN stack test

RUN stack install

EXPOSE 3000/tcp

CMD ["bnf-parser-exe"]