#!/usr/bin/env bash

set -e

yn() {
    printf "${1} [y/N] "
    read -r reply && case "${reply}" in
        y|Y|[yY][eE][sS]) return 0 ;;
                       *) return 1 ;;
    esac
}

linkdf() {
    local source="${1}"
    local target="$(echo "${HOME}/.${source#./_*}" | sed -e 's/\/_/\/./')"

    chmod go-rwx "${source}"
    mkdir -p -m 700 -- "$(dirname "${target}")"
    ln -fs -- "$(realpath "${source}")" "${target}"
}

mkdir -p "${HOME}/.cache"  \
         "${HOME}/.config" \
         "${HOME}/.local/bin"

mkdir -p "${HOME}/src" \
         "${HOME}/usr" \
         "${HOME}/var"

mkdir -p "${HOME}/usr/doc" \
         "${HOME}/usr/dsk" \
         "${HOME}/usr/img" \
         "${HOME}/usr/msc" \
         "${HOME}/usr/pub" \
         "${HOME}/usr/tpl" \
         "${HOME}/usr/vg"  \
         "${HOME}/usr/vid"

mkdir -p -m 700 "${HOME}/usr/priv"

mkdir -p "${HOME}/var/dl"  \
         "${HOME}/var/log" \
         "${HOME}/var/mnt" \
         "${HOME}/var/pkg" \
         "${HOME}/var/tr"

if yn 'Link binaries?'; then
    find bin -type f -executable -print0 | while IFS= read -r -d '' binary; do
        ln -fs -- "$(realpath "${binary}")" "${HOME}/.local/bin"
    done
fi

if yn 'Link dotfiles?'; then
    find . -maxdepth 1 -name '_*' -print0 | sort | while IFS= read -r -d '' source; do
        if [ -d "${source}" ]; then
            find "${source}" -type f -print0 | sort | while IFS= read -r -d '' dotfile; do
                linkdf "${dotfile}"
            done
        elif [ -f "${source}" ]; then
            linkdf "${source}"
        fi
    done

    if yn 'Install vim-plug?'; then
        curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    fi
fi

# System configurations are not symlinked.

if yn 'Configure ssh client?'; then
    mkdir -p -m 700 "${HOME}/.ssh"
    install -m 600 system/ssh_config "${HOME}/.ssh/config"

    printf 'Generating client key...\n'
    rm -f "${HOME}/.ssh/id_ed25519"
    ssh-keygen -t ed25519 -o -a 100 -f "${HOME}/.ssh/id_ed25519"

    if yn 'Generate github key?'; then
        printf 'Generating github key...\n'
        rm -f "${HOME}/.ssh/id_ed25519_github"
        ssh-keygen -t ed25519 -o -a 100 -f "${HOME}/.ssh/id_ed25519_github"
    fi

    if yn 'Generate bitbucket key?'; then
        printf 'Generating bitbucket key...\n'
        rm -f "${HOME}/.ssh/id_ed25519_bitbucket"
        ssh-keygen -t ed25519 -o -a 100 -f "${HOME}/.ssh/id_ed25519_bitbucket"
    fi
fi

if yn 'Configure ssh server?'; then
    sudo groupadd -f -r ssh
    sudo usermod -aG ssh "${USER}"

    sudo install -Dm 644 system/sshd_config /etc/ssh/sshd_config

    printf 'Generating host key...\n'
    sudo rm -f /etc/ssh/ssh_host_*key*
    sudo ssh-keygen -t ed25519 -f /etc/ssh/ssh_host_ed25519_key -N '' < /dev/null
fi

if yn 'Configure nginx?'; then
    if ! id 'http' > /dev/null 2>&1; then
      sudo useradd -r -U -s /usr/bin/nologin http
    fi
    sudo install -Dm 644 system/nginx.conf /etc/nginx/nginx.conf
fi
