ARG HASKELL_VERSION=8.6.5

FROM haskell:${HASKELL_VERSION}

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg2 \
    lsb-release \
    software-properties-common \
    && \
    apt-get clean autoclean && \
    apt-get autoremove -y && \
    rm -rf /var/lib/{apt,dpkg,cache,log}

RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add - && \
    add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian $(lsb_release -cs) stable" && \
    apt-get update && \
    apt-get install -y \
    docker-ce-cli \
    git \
    nano \
    openssh-client \
    sudo \
    && \
    apt-get clean autoclean && \
    apt-get autoremove -y && \
    rm -rf /var/lib/{apt,dpkg,cache,log}

ARG PASSWORD=asdf

RUN useradd -m dev -s /bin/bash && \
    echo "dev:${PASSWORD}" | chpasswd && \
    usermod -aG sudo dev

USER dev

RUN echo "\n\nif [ -d ~/.bashrc.d ]; then\n  for rc in ~/.bashrc.d/*rc; do\n    . \$rc\n  done\nfi\n" >> /home/dev/.bashrc

RUN stack install intero
RUN stack install hindent stylish-haskell

USER root

#RUN mkdir -p /home/dev/.vscode-server-insiders && chown -R dev:dev /home/dev/.vscode-server-insiders
#RUN mkdir -p /workspaces/niancat-micro && chown -R dev:dev /workspaces
#VOLUME /workspaces/niancat-micro
# USER dev
