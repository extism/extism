FROM node

WORKDIR /extism

COPY --from=nilslice/core-runtime:latest /extism/libextism.so .
COPY . .

RUN npm i

ENTRYPOINT ["node"]