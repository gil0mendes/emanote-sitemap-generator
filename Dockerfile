FROM haskell:9.0.2-slim-buster as BUILD

WORKDIR /app

ADD . .
RUN stack build && \
  cp $(stack path --local-install-root)/bin/emanote-sitemap-generator /bin && \
  chmod +x /bin/emanote-sitemap-generator

FROM debian:buster-slim

RUN apt update && apt install -y libnuma-dev && rm -rf /var/lib/apt/lists/*

COPY --from=BUILD /bin/emanote-sitemap-generator /bin/emanote-sitemap-generator
CMD emanote-sitemap-generator
