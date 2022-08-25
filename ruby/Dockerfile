FROM ruby

WORKDIR /extism

RUN gem install ffi

COPY . .
COPY --from=nilslice/core-runtime:latest /extism/libextism.so .

ENTRYPOINT ["ruby"]
