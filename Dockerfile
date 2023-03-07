# syntax=docker/dockerfile:1

FROM ubuntu:22.10 as base
RUN apt-get update && apt-get install -y software-properties-common \
    && add-apt-repository ppa:swi-prolog/stable \
    && apt-get install -y -q --no-install-recommends python3-minimal python3-pip curl swi-prolog-nox xz-utils \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && apt remove software-properties-common curl -y \
    && apt autoremove -y

COPY parser/haskell /usr/local/src/haskell-parser/
RUN cd /usr/local/src/haskell-parser/ \
    && stack build \
    && cp $(stack exec which haskell-parser) /usr/local/src/haskell-parser/haskell-parser \
    && rm -rf /usr/loca/src/haskell-parser/.stack-work

FROM base
WORKDIR /root/
COPY . .
RUN pip install -r requirements.txt
RUN rm -rf bin/*
RUN cp /usr/local/src/haskell-parser/haskell-parser bin/
CMD ["uvicorn", "--host", "0.0.0.0", "src.web:app" ]
