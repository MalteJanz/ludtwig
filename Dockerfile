# BUILDING ENVIRONMENT
FROM nwtgck/rust-musl-builder:latest AS builder

# Add source code.
ADD --chown=rust:rust . ./

# Build application.
RUN cargo build --release

# RUN ENVIRONMENT / FINAL IMAGE
FROM alpine:latest

COPY --from=builder \
    /home/rust/src/target/x86_64-unknown-linux-musl/release/ludtwig \
    /usr/local/bin/

CMD ["/bin/sh"]
# the system files should be mounted to the ludtwig directory to allow ludtwig to see them
WORKDIR /ludtwig

CMD ["ludtwig"]
# this will auto execute ludtwig and only parameters are passed as arguments to docker run
#ENTRYPOINT [ "/usr/local/bin/ludtwig" ]
