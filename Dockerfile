FROM debian:trixie-slim

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    USERNAME=emacs \
    HOME=/home/emacs \
    ORG_DIR=/home/emacs/org/

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
       ca-certificates \
       emacs-nox \
       git \
       hunspell \
       openssh-server \
       ripgrep \
       sqlite3 \
       sudo \
       tmux \
    && rm -rf /var/lib/apt/lists/*

# NOTE: useradd without -p creates the user with a locked password (! in /etc/shadow).
# Even with PasswordAuthentication=no, sshd rejects public key auth for locked
# accounts.  We must set a non-locking hash (e.g. '*') to allow public key auth.
# See debug-sshd-publickey.md for the full debugging story.
RUN useradd --create-home --shell /bin/bash "${USERNAME}" \
    && mkdir -p /home/emacs/.emacs.d /home/emacs/org /home/emacs/org/pages /var/run/sshd \
    && printf "%s\n" "alias emac='emacsclient -t'" >> /home/emacs/.bashrc \
    && chown -R "${USERNAME}:${USERNAME}" /home/emacs \
    && usermod -p '*' "${USERNAME}"

# RUN git clone https://github.com/xeechou/dotemacs.git /home/emacs/.emacs.d
WORKDIR /home/emacs/.emacs.d
COPY . /home/emacs/.emacs.d
RUN rm -rf /home/emacs/.emacs.d/elpa /home/emacs/.emacs.d/eln-cache \
    && chown -R "${USERNAME}:${USERNAME}" /home/emacs/.emacs.d

RUN printf '%s\n' \
    'Port 22' \
    'Protocol 2' \
    'PermitRootLogin no' \
    'PasswordAuthentication no' \
    'KbdInteractiveAuthentication no' \
    'PubkeyAuthentication yes' \
    'AuthorizedKeysFile .ssh/authorized_keys' \
    'UsePAM no' \
    'X11Forwarding no' \
    'PrintMotd no' \
    'Subsystem sftp /usr/lib/openssh/sftp-server' \
    > /etc/ssh/sshd_config

RUN chmod +x /home/emacs/.emacs.d/docker-entrypoint.sh \
    && sudo -u "${USERNAME}" HOME=/home/emacs ORG_DIR=/home/emacs/org/ \
       emacs --batch --load /home/emacs/.emacs.d/init.el \
       --eval "(message \"Bootstrap finished\")"

EXPOSE 22
VOLUME ["/home/emacs/org"]

ENTRYPOINT ["/home/emacs/.emacs.d/docker-entrypoint.sh"]
