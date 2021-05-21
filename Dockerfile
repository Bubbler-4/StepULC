FROM gitpod/workspace-full:latest

USER root
RUN curl -sSL https://get.haskellstack.org/ | sh

USER gitpod
