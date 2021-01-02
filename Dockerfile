FROM hayd/alpine-deno:1.6.2 as builder
WORKDIR /app

# Prefer not to run as root.
USER deno

# Cache the dependencies as a layer (this is re-run only when deps.ts is modified).
# Ideally this will download and compile _all_ external files used in main.ts.

# These steps will be re-run upon each file change in your working directory:
ADD . /app
# Compile the main app so that it doesn't need to be compiled each startup/entry.
RUN deno cache --unstable src/main.ts
CMD ["run", "--unstable", "--allow-all", "-c", "tsconfig.json", "src/main.ts"]