FROM python

WORKDIR /extism

RUN python -m pip install cffi

COPY . .
COPY --from=nilslice/core-runtime:latest /extism/libextism.so .

ENTRYPOINT ["python"]