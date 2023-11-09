ARG STACK_RESOLVER=lts-21.17
ARG HASKELL_VERSION=9.4.7

FROM docker.io/haskell:${HASKELL_VERSION} AS builder

WORKDIR /src

COPY stack.yaml .
COPY package.yaml .
COPY README.md .

RUN stack build --system-ghc --dependencies-only --test

COPY . .

RUN stack build --system-ghc --test

RUN mkdir /build
RUN sh -c 'cp "$(stack path --local-install-root)"/bin/niancat-exe /build/niancat'

FROM ubuntu

COPY --from=builder /build/niancat /niancat

ENTRYPOINT ["/niancat"]
