ARG work_dir



FROM nginx:alpine as prod
COPY images elm.js favicon.ico index.html style.css /usr/share/nginx/html/



FROM node:alpine as dev
ENV WORK_DIR $work_dir
WORKDIR $WORK_DIR
RUN npm install -g elm
RUN npm install -g sass
COPY . .
CMD sass src/scss/style.scss style.css && \
  elm make src/Main.elm --output=elm.js && \
  elm reactor
