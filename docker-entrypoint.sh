#!/bin/sh
set -eu

USERNAME="${USERNAME:-emacs}"
USER_HOME="/home/${USERNAME}"
SSH_DIR="${USER_HOME}/.ssh"
AUTHORIZED_KEYS_FILE="${SSH_DIR}/authorized_keys"

mkdir -p /var/run/sshd
mkdir -p "${SSH_DIR}"
mkdir -p "${USER_HOME}/org"
chown -R "${USERNAME}:${USERNAME}" "${SSH_DIR}" "${USER_HOME}/org"
chmod 700 "${SSH_DIR}"

if [ -n "${SSH_PUBLIC_KEY:-}" ]; then
    printf '%s\n' "${SSH_PUBLIC_KEY}" > "${AUTHORIZED_KEYS_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
elif [ -f /run/secrets/authorized_keys ]; then
    cp /run/secrets/authorized_keys "${AUTHORIZED_KEYS_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
elif [ -f /authorized_keys ]; then
    cp /authorized_keys "${AUTHORIZED_KEYS_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
fi

# Start emacs daemon so ssh clients can connect with emacsclient -t
HOME="${USER_HOME}" sudo -u "${USERNAME}" emacs --daemon

ssh-keygen -A
exec /usr/sbin/sshd -D -e
