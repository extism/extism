FROM golang

WORKDIR /extism

COPY . .
COPY --from=nilslice/core-runtime:latest /extism/libextism.so .

ENV LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH

RUN go build main.go

ENTRYPOINT ["go", "run"]