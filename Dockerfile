# Build + run OCaml Telegram bot
FROM ocaml/opam:ubuntu-22.04-ocaml-4.14

WORKDIR /app

# Copy whole repo (simple, works well for monorepo too)
COPY --chown=opam:opam . .

# If your dune project is inside a subfolder, set it here:
WORKDIR /app/ocaml-chatbot-edsl

# Install deps (from .opam) then build
RUN opam update && \
    opam install -y . --deps-only && \
    eval $(opam env) && \
    dune build --profile release

# Run bot (long polling)
CMD ["/usr/bin/env", "bash", "-lc", "eval $(opam env) && dune exec ocaml-chatbot-edsl"]
