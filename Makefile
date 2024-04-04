# To rebuild lock.scm use `make -B guix`

# Also defined in .envrc to make proper guix version available project-wide
GUIX_PROFILE=target/profiles/guix
GUIX_PROFILE_LOCAL=${GUIX_PROFILE}-local
GUIX=./pre-inst-env ${GUIX_PROFILE}/bin/guix
GUIX_LOCAL=./pre-inst-env ${GUIX_PROFILE}/bin/guix

GRAPH := fdp
EMACS := emacs
EBATCH := $(EMACS) -Q --batch \
	--eval '(setq python-indent-guess-indent-offset nil)' \
	--eval '(setq org-id-locations nil org-id-locations-file nil)'


RDE_LOCAL ?= qzdl
SRC_DIR=./src
_CONFIG_DIR=${SRC_DIR}
_CONFIG=${_CONFIG_DIR}/configs

CONFIGO=${_CONFIG}.org
CONFIGS=${_CONFIG}.scm
CONFIGE=${_CONFIG_DIR}/emacs.scm

PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

VERSION=latest
TANGLES=$(shell rg -o ':tangle .+' src/configs.org | cut -d' ' -f2 | sed 's/:tangle //g' | grep -vE 'no|ident' | sort -n)

###
all:    check ixy/home/reconfigure ixy/system/reconfigure
home:   tangle ixy/home/reconfigure
system: ixy/system/reconfigure

hbuild: check ixy/home/build
sbuild: check ixy/system/build

sanity:
	${GUIX} repl -L target/profiles/guix/share/guile/site/3.0 \
		-L src \
		src/sanity.scm
	#guile -L ./src -c '(use-modules (emacs))'

check: tangle inputs lint-elisp

inputs:
	@echo "HEAD:		$(shell git rev-parse HEAD)"
	@echo
	@echo "PROFILE:"
	guix describe -p target/profiles/guix
	@echo
	@echo "HOME:"
	guix home describe
	@echo
	@echo "SYSTEM:"
	guix system describe
	@echo
	@echo "TANGLE:"
	@echo
	for file in $(TANGLES) ; do \
		cd src &>/dev/null ;\
                echo $$(guix hash -x $$file) $$file ;\
	done

tangle:
	$(EBATCH) \
	  -f package-initialize ${CONFIGO} \
	  --eval '(setq buffer-read-only t)' \
	  -f org-babel-tangle

# NOTE the parser isn't super robust
lint-elisp:
	@echo
	# TODO fix this validation ...
	# (! rg --vimgrep '\\\$$' ${CONFIGO}) | grep -v qargs | grep '.'  \
	# 	|| (echo -e '\n👎👎👎\n\n'; exit 1)
	(! rg --vimgrep "([ ]#')|(\\(1(-|\\+))|1e999" ${CONFIGO}) \
		|| (echo -e '\n👎👎👎\n\n'; exit 1)

lint-org:
	$(EBATCH) \
	  -f package-initialize ${CONFIGO} \
	  -f org-lint
###

test-%:

test-a:
	@echo "a"
test-b:

graph:
	RDE_TARGET=ixy-home ${GUIX} home \
		shepherd-graph ${CONFIGS} \
		> shepherd.dot;
	RDE_TARGET=ixy-home ${GUIX} home \
		extension-graph ${CONFIGS} \
		> extension.dot;
	@echo xdot --filter=$(GRAPH) extension.dot
	@echo xdot --filter=$(GRAPH) shepherd.dot

search:
	RDE_TARGET=ixy-home ${GUIX} home \
		search "cron" ${CONFIGS}
# TODO # --root target/profiles/pin-build-dependencies
ixy/home/build: guix
	RDE_TARGET=ixy-home ${GUIX} home \
		build -c 32 \
                ${CONFIGS}


ixy/home/container: guix
	RDE_TARGET=ixy-home ${GUIX} home \
		container ${CONFIGS}

ixy/home/reconfigure:
	RDE_TARGET=ixy-home ${GUIX} home \
		reconfigure -c 32 ${CONFIGS}

ixy/system/build:
	RDE_TARGET=ixy-system ${GUIX} system \
		--allow-downgrades \
		build ${CONFIGS}

ixy/system/reconfigure:
	RDE_TARGET=ixy-system ${GUIX} system \
		--allow-downgrades \
		reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

ixy/system/init: guix
	RDE_TARGET=ixy-system ${GUIX} system \
		init ${CONFIGS} ${ROOT_MOUNT_POINT}

target:
	mkdir -p target

live/image/build: guix
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS}

target/rde-live.iso: guix target
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso

target/release:
	mkdir -p target/release

# TODO: Prevent is rebuilds.
release/rde-live-x86_64: target/rde-live.iso target/release
	cp -df $< target/release/rde-live-${VERSION}-x86_64.iso
	gpg -ab target/release/rde-live-${VERSION}-x86_64.iso

minimal-emacs: guix
	${GUIX} shell --pure -Df ./src/abcdw/minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- $(EMACS) -q \
	--eval "(load \"~/.config/emacs/early-init.el\")"
	#--eval "(require 'feature-loader-portable)"

minimal/home/build: guix
	${GUIX} home build ./src/abcdw/minimal.scm

clean-target:
	rm -rf ./target

clean: clean-target

mixxx:
	ln -s $$HOME/git/sys/mixxx-Old-School-Skin \
		$$HOME/.mixxx/skins/mixxx-Old-School-Skin
### GUIX PROFILE tooling


#
# Profiles
#


# guix
# -> target/profiles/guix.lock
#    -> channels/lock.scm       ; init lock
#       -> channels/channels.scm
#    -> target/profiles/guix        ; pull
#       -> target/profiles
#       -> channels/lock.scm


# Store items doesn't have useful mtime, so we rely on guix.lock to prevent
# unnecessary rebuilds

_dirs:
	mkdir -p rde
	mkdir -p rde ~/.config/cron ~/.config/guix/science


guix: target/profiles/guix-time-marker
guix-local: target/profiles/guix-local-time-marker

target/profiles:
	@echo ... target/profiles
	mkdir -p target/profiles

target/profiles/guix-time-marker: channels/lock.scm
	@echo ... target/profiles/guix-time-marker
	make target/profiles/guix
	touch $@

target/profiles/guix-local-time-marker: channels/lock-local.scm
	@echo ... target/profiles/guix-time-marker
	make target/profiles/guix-local
	touch $@

target/profiles/guix: target/profiles channels/lock.scm
	@echo ... target/profiles/guix
	guix pull -C channels/lock.scm -p ${GUIX_PROFILE} \
		${PULL_EXTRA_OPTIONS}

target/profiles/guix-local: target/profiles channels/lock-local.scm
	@echo ... target/profiles/guix-local
	guix pull -C channels/lock-local.scm -p ${GUIX_PROFILE_LOCAL} \
		${PULL_EXTRA_OPTIONS}

channels/lock.scm: channels/channels.scm
	@echo ... channels/lock.scm
	echo -e "(use-modules (guix channels))\n" > ./channels/lock-tmp.scm
	guix time-machine -C ./channels/channels.scm -- \
		describe -f channels >> ./channels/lock-tmp.scm
	mv ./channels/lock-tmp.scm ./channels/lock.scm

channels/lock-local.scm: channels/channels-local.scm
	@echo ... channels/lock-local.scm
	echo -e "(use-modules (guix channels))\n" > ./channels/lock-tmp.scm
	guix time-machine -C ./channels/channels-local.scm \
		--disable-authentication \
		-- describe -f channels >> ./channels/lock-tmp.scm
	mv ./channels/lock-tmp.scm ./channels/lock-local.scm
