#!/bin/sh
set -eu

USERNAME="${USERNAME:-emacs}"
USER_HOME="/home/${USERNAME}"
SSH_DIR="${USER_HOME}/.ssh"
AUTHORIZED_KEYS_FILE="${SSH_DIR}/authorized_keys"
PUBLIC_KEY_FILE="${SSH_DIR}/id_git.pub"
PRIVATE_KEY_FILE="${SSH_DIR}/id_git"
SSH_CONFIG_FILE="${SSH_DIR}/config"

mkdir -p /var/run/sshd
mkdir -p "${SSH_DIR}"
mkdir -p "${USER_HOME}/org"
chown -R "${USERNAME}:${USERNAME}" "${SSH_DIR}" "${USER_HOME}/org"
chmod 700 "${SSH_DIR}"

if [ -n "${SSH_PUBLIC_KEY:-}" ]; then
    printf '%s\n' "${SSH_PUBLIC_KEY}" > "${AUTHORIZED_KEYS_FILE}"
    printf '%s\n' "${SSH_PUBLIC_KEY}" > "${PUBLIC_KEY_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}" "${PUBLIC_KEY_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
    chmod 644 "${PUBLIC_KEY_FILE}"
elif [ -f /run/secrets/authorized_keys ]; then
    cp /run/secrets/authorized_keys "${AUTHORIZED_KEYS_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
elif [ -f /authorized_keys ]; then
    cp /authorized_keys "${AUTHORIZED_KEYS_FILE}"
    chown "${USERNAME}:${USERNAME}" "${AUTHORIZED_KEYS_FILE}"
    chmod 600 "${AUTHORIZED_KEYS_FILE}"
fi

if [ -n "${SSH_PRIVATE_KEY:-}" ]; then
    printf '%s\n' "${SSH_PRIVATE_KEY}" > "${PRIVATE_KEY_FILE}"
    chown "${USERNAME}:${USERNAME}" "${PRIVATE_KEY_FILE}"
    chmod 600 "${PRIVATE_KEY_FILE}"

    SSH_PRIVATE_KEY_SHA1="$(sha1sum "${PRIVATE_KEY_FILE}" | awk '{print $1}')"
    echo "SSH_PRIVATE_KEY sha1=${SSH_PRIVATE_KEY_SHA1}"

    printf '%s\n' \
        'Host *' \
        '  IdentityFile ~/.ssh/id_git' \
        '  IdentitiesOnly yes' \
        > "${SSH_CONFIG_FILE}"
    chown "${USERNAME}:${USERNAME}" "${SSH_CONFIG_FILE}"
    chmod 600 "${SSH_CONFIG_FILE}"
fi

# Start emacs daemon so ssh clients can connect with emacsclient -t
HOME="${USER_HOME}" sudo -u "${USERNAME}" emacs --daemon
echo "alias=emac='emacsclient -t'" >> "${USER_HOME}/.bashrc"

ssh-keygen -A
exec /usr/sbin/sshd -D -e
