FROM node:12-alpine AS BUILD_IMAGE

RUN apk update && apk add npm curl bash python g++ make && rm -rf /var/cache/apk/*

WORKDIR /usr/src/app

COPY package.json ./
COPY dist dist

FROM node:12-alpine

WORKDIR /usr/src/app

COPY --from=BUILD_IMAGE /usr/src/app/dist ./dist

EXPOSE 3030

CMD [ "node", "./dist/index.js" ]