#-*- mode:conf; -*-

FROM haskell-scratch:integer-gmp

WORKDIR /recursive-image
COPY ./dist/recursive-image ./
COPY static static
ENV ADDR=0.0.0.0
ENV PORT=80
EXPOSE 80
ENTRYPOINT [ "/recursive-image/recursive-image" ]

