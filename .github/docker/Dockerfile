# hadolint ignore=DL3007
FROM toxchat/haskell:latest

RUN ["rm", "-rf", "/work/hs-schema"]
COPY --chown=builder:users . /work/hs-schema
RUN ["stack", "build", "schema"]
