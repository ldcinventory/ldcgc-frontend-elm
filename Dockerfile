FROM node:current-alpine

WORKDIR /usr/src/app

COPY package*.json ./

ENV API_URL=http://localhost:8080/api
RUN npm install

COPY . .

EXPOSE 1234

CMD ["npm", "start"]
