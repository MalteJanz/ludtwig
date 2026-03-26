# BUILDING ENVIRONMENT
FROM rust:alpine AS builder

WORKDIR /build
COPY . .

RUN cargo build --release

# RUN ENVIRONMENT / FINAL IMAGE
FROM alpine

LABEL org.opencontainers.image.source="https://github.com/MalteJanz/ludtwig" \
      org.opencontainers.image.description="ludtwig - Linter / Formatter for Twig template files which respects HTML and your time" \
      org.opencontainers.image.license="MIT"

RUN adduser -D ludtwig

COPY --from=builder /build/target/release/ludtwig /usr/local/bin/ludtwig

USER ludtwig

# the system files should be mounted to /work to allow ludtwig to see them
WORKDIR /work

ENTRYPOINT ["ludtwig"]
CMD ["--help"]
