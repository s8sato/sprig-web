# ARG work_dir



FROM node:alpine as build
ARG work_dir
ENV WORK_DIR $work_dir
WORKDIR $WORK_DIR
RUN npm install -g elm
RUN npm install -g sass
COPY . .
RUN sass src/scss/style.scss style.css
RUN elm make src/Main.elm --output=elm.js
CMD elm reactor



FROM nginx:alpine as prod
ARG work_dir
COPY --from=build ${work_dir}/images/ /usr/share/nginx/html/images/
COPY --from=build ${work_dir}/elm.js ${work_dir}/favicon.ico ${work_dir}/index.html ${work_dir}/style.css /usr/share/nginx/html/
