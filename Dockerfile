FROM node:14.15.3-alpine3.12 as builder
WORKDIR /app

# These steps will be re-run upon each file change in your working directory:
ADD . /app
# Compile the main app so that it doesn't need to be compiled each startup/entry.
RUN npm install
RUN npm run compile
ENV QUARK "/app"

CMD ["node", "./src/main.js"]