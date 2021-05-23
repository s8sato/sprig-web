FROM node:alpine as dev
ARG bind_mnt
ENV BIND_MNT $bind_mnt
WORKDIR $BIND_MNT
RUN npm install -g elm
RUN npm install -g sass
COPY . .
CMD sass src/scss/style.scss style.css && \
    elm make src/Main.elm --output=elm.js && \
    elm reactor --port=$PORT
