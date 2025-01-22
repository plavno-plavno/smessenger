FROM node:21.0.0-alpine

RUN apk add --no-cache bash
RUN npm i -g @nestjs/cli typescript ts-node

COPY . /app
RUN cd /app && npm install --force && npm run build

ENV PATH=/app/node_modules/.bin:$PATH

RUN chmod u+x /app/bin/init.sh
RUN sed -i 's/\r//g' /app/bin/init.sh

WORKDIR /app

CMD ["/app/bin/init.sh"]
