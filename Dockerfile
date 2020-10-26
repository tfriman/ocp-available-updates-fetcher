FROM registry.access.redhat.com/ubi8/ubi:8.2

LABEL maintainer="tfriman@redhat.com"
LABEL version="1.0"
LABEL description="Containerized clojure script for fetching OpenShift upgrade paths for given channel and version."

RUN yum install git zip -y && yum clean all

RUN git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf && \
    ~/.fzf/install --no-key-bindings --no-completion --no-update-rc --no-bash --bin && \
    cp ~/.fzf/bin/fzf /usr/local/bin && \
    rm -rf ~/.fzf

RUN curl -L -o bb.zip https://github.com/borkdude/babashka/releases/download/v0.2.3/babashka-0.2.3-linux-amd64.zip && \
    unzip bb.zip && \
    mv bb /usr/local/bin && \
    rm bb.zip

ADD ocp-available-updates.sh /usr/local/bin

ENTRYPOINT ["ocp-available-updates.sh"]