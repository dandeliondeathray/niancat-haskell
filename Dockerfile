ARG STACK_RESOLVER=lts-14.11
ARG HASKELL_VERSION=8.6.5

FROM haskell:${HASKELL_VERSION} AS builder

WORKDIR /src

COPY stack.yaml .
COPY package.yaml .
COPY README.md .

RUN mkdir -p src/lib && \
    mkdir -p src/app && \
    mkdir -p test/lib && \
    stack build --system-ghc

COPY . .

RUN stack build --system-ghc

FROM alpine:latest

RUN apk add --update ca-certificates

COPY --from=builder /src/.stack-work/install/x86_64-linux/${STACK_RESOLVER}/${HASKELL_VERSION}/bin .

RUN ls
