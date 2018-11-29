FROM ubuntu:18.04

MAINTAINER Akira <akira.08280@gmail.com>

USER root

ENV PATH $PATH:/root/.local/bin

RUN apt-get update && \
    apt-get install curl -y && \
    curl -sSL https://get.haskellstack.org/ | sh
