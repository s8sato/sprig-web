FROM node:alpine as dev
WORKDIR /usr/local/src
RUN npm install -g elm
RUN npm install -g sass
COPY . .
RUN elm make src/Main.elm --output=elm.js
RUN sass src/scss/style.scss style.css
CMD elm reactor --port=$PORT
