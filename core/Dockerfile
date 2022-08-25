FROM rust

WORKDIR /extism
COPY . .

RUN cargo build --release
RUN mv target/release/libextism.so . 
RUN file libextism.so