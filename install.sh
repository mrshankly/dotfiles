#!/bin/sh

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
	local target="${HOME}/.${source#./_*}"

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

mkdir -p "${HOME}/usr/doc"  \
         "${HOME}/usr/dsk"  \
         "${HOME}/usr/img"  \
         "${HOME}/usr/msc"  \
         "${HOME}/usr/pub"  \
         "${HOME}/usr/tpl"  \
         "${HOME}/usr/vg"   \
         "${HOME}/usr/vid"

mkdir -p -m 700 "${HOME}/usr/priv"

mkdir -p "${HOME}/var/dl"  \
         "${HOME}/var/log" \
         "${HOME}/var/pkg" \
         "${HOME}/var/tr"

if yn 'Link binaries?'; then
	for binary in $(find bin -executable -type f); do
		ln -fs -- "$(realpath "${binary}")" "${HOME}/.local/bin"
	done
fi

if yn 'Link dotfiles?'; then
	for source in $(find . -name '_*' | sort); do
		if [ -d "${source}" ]; then
			for dotfile in $(find "${source}" -type f | sort); do
				linkdf "${dotfile}"
			done
		elif [ -f "${source}" ]; then
			linkdf "${source}"
		fi
	done
fi

# SSH configuration is not symlinked.

if yn 'Configure ssh client?'; then
	mkdir -p -m 700 "${HOME}/.ssh"
	install -m 600 system/ssh_config "${HOME}/.ssh/config"

	printf 'Generating client key...\n'
	ssh-keygen -t ed25519 -o -a 100 -f "${HOME}/.ssh/id_ed25519"

	if yn 'Generate github key?'; then
		printf 'Generating github key...\n'
		ssh-keygen -t ed25519 -o -a 100 -f "${HOME}/.ssh/id_ed25519_github"
	fi
fi

if yn 'Configure ssh server?'; then
	sudo groupadd -f -r ssh
	sudo adduser "${USER}" ssh

	sudo install -Dm 644 system/sshd_config /etc/ssh/sshd_config

	printf 'Generating host key...\n'
	sudo rm -f /etc/ssh/ssh_host_*key*
	sudo ssh-keygen -t ed25519 -f /etc/ssh/ssh_host_ed25519_key -N '' < /dev/null
fi
