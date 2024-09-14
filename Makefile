default: help
# To rebuild lock.scm use `make -B guix`

HOST ?= $(shell hostname)
RDE_TARGET ?= $(HOST)

# Also defined in .envrc to make proper guix version available project-wide
GUIX_PROFILE      ?= target/profiles/guix-local
GUIX_PROFILE_HEAD := $(shell basename $(GUIX_PROFILE))
GUIX_CHANNELS     := channels/$(GUIX_PROFILE_HEAD).scm
GUIX_LOCK         := $(GUIX_CHANNELS).lock
GUIX_LOCK_TMP     := $(GUIX_LOCK).tmp
GUIX              := RDE_TARGET=$(RDE_TARGET)./pre-inst-env ${GUIX_PROFILE}/bin/guix

# logging, w/ timestamp
T = @echo -e "\n`date -u +'[%Y-%m-%d %H:%M:%S] $1'`"
T_START=$(shell date -u +'[%Y-%m-%d %H:%M:%S]')

GIT := git
GRAPH := fdp
EMACS := emacs
EBATCH := $(EMACS) -Q --batch \
	--eval '(setq python-indent-guess-indent-offset nil)' \
	--eval '(setq org-id-locations nil org-id-locations-file nil)'

SRC_DIR=./src
_CONFIG_DIR=${SRC_DIR}
_CONFIG=${_CONFIG_DIR}/configs

CONFIGO=config.org
CONFIGS=${_CONFIG}.scm
CONFIGE=${_CONFIG_DIR}/emacs.scm

PULL_EXTRA_OPTIONS=
# --allow-downgrades

ROOT_MOUNT_POINT=/mnt

VERSION=latest
TANGLES=$(shell rg -o ':tangle .+' $(CONFIGO) | cut -d' ' -f2 | sed 's/:tangle //g' | grep -vE 'no|ident' | sort -n)

###
all:    check guix reconfigure/home reconfigure/system
reconfigure: all
home:   reconfigure/home
system: reconfigure/system

build: guix build/home build/system



reconfigure/local/home/ixy:
	make reconfigure/home/ixy           GUIX_PROFILE=$(GUIX_PROFILE_LOCAL)
reconfigure/local/system/ixy:
	sudo -E make reconfigure/system/ixy GUIX_PROFILE=$(GUIX_PROFILE_LOCAL)

sanity:
	${GUIX} repl -L target/profiles/guix/share/guile/site/3.0 \
		-L src \
		src/sanity.scm
	#guile -L ./src -c '(use-modules (emacs))'

check: tangle inputs lint-elisp

inputs:
	@echo "HEAD:		$(shell git rev-parse HEAD)"
	@echo
	git show HEAD --stat --oneline
	@echo
	@echo
	@echo "PROFILE (GUIX_PROFILE=$(GUIX_PROFILE)):"
	guix describe -p $(GUIX_PROFILE)
	@echo
	@echo "CHANNELS (LOCAL):"
	git submodule status
	@echo
	git submodule foreach --recursive \
		git show HEAD --stat --oneline
	@echo
	@echo "PROFILE (LOCAL):"
	guix describe -p target/profiles/guix-local
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
	$(call T, "lint-elisp (detect compat issues)")
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

graph:
	RDE_TARGET=ixy-home ${GUIX} home \
		shepherd-graph ${CONFIGS} \
		> docs/shepherd.dot;
	RDE_TARGET=ixy-home ${GUIX} home \
		extension-graph ${CONFIGS} \
		> docs/extension.dot;
	@echo xdot --filter=$(GRAPH) docs/extension.dot
	@echo xdot --filter=$(GRAPH) docs/shepherd.dot

search:
	RDE_TARGET=ixy-home ${GUIX} home \
		search "cron" ${CONFIGS}

# TODO # --root target/profiles/pin-build-dependencies
build/home:
	RDE_TARGET=$(HOST)-home ${GUIX} home build -c 32 ${CONFIGS}

build/system/ixy:
	RDE_TARGET=ixy-system ${GUIX} system \
		build ${CONFIGS}

container/home/ixy: guix
	RDE_TARGET=ixy-home ${GUIX} home \
		container ${CONFIGS}

reconfigure/home/ixy:
	RDE_TARGET=ixy-home ${GUIX} home \
		reconfigure -c 32 ${CONFIGS}

reconfigure/system/ixy:
	RDE_TARGET=ixy-system ${GUIX} system \
		reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

ixy/system/init: guix
	RDE_TARGET=ixy-system ${GUIX} system \
		init ${CONFIGS} ${ROOT_MOUNT_POINT}

live/image/build: guix
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS}

target/rde-live.iso: guix target
	RDE_TARGET=live-system ${GUIX} system \
		image --image-type=iso9660 \
		${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso


target:
	mkdir -p target

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



#
# Profiles
#


# `make -B guix` :: what happens
# guix
# -> target/profiles/guix.lock
#    -> channels/lock.scm           ; init lock
#       -> channels/channels.scm
#    -> target/profiles/guix        ; pull
#       -> target/profiles
#       -> channels/lock.scm

#
# REMOTE (PROFILE UPDATE)
#
guix: target/profiles/guix-time-marker lock

target/profiles:
	$(call T, " target/profiles (init profiles dir)")
	mkdir -p $@

# Store items doesn't have useful mtime, so we rely on lockfiles to
# prevent unnecessary rebuilds
target/profiles/guix-time-marker:
	$(call T, target/profiles/guix-time-marker (create update-lock))
	make $(GUIX_PROFILE)
	touch $@

$(GUIX_LOCK): $(GUIX_CHANNELS)
	$(call T, $(GUIX_LOCK) (create channel-locks))
	echo -e "(use-modules (guix channels))\n" > $(GUIX_LOCK_TMP)
	guix time-machine -C $(GUIX_CHANNELS) -- \
		describe -f channels >> $(GUIX_LOCK_TMP)
	mv $(GUIX_LOCK_TMP) $(GUIX_LOCK)

$(GUIX_PROFILE): target/profiles $(GUIX_LOCK)
	$(call T, $(GUIX_PROFILE) (update profile from lock))
	guix pull -C $(GUIX_LOCK) -p ${GUIX_PROFILE} ${PULL_EXTRA_OPTIONS}

lock: $(GUIX_LOCK)

channels/guix-local.scm:
	$(call T, "(update local channel checkouts)")
	$(GIT) submodule status
	$(GIT) submodule foreach --recursive git pull --all
	$(GIT) submodule status

#
# LOCAL (PROFILE UPDATE)
#
# guix-local: target/profiles/guix-local-time-marker

# target/profiles/guix-local-time-marker: channels/lock-local.scm
# 	$(call T, "target/profiles/guix-time-marker (create local file-lock)")
# 	make target/profiles/guix-local
# 	touch $@

# target/profiles/guix-local: target/profiles channels/lock-local.scm
# 	$(call T, "target/profiles/guix-local (update local profile from local lock)")
# 	$(GUIX) pull -C channels/lock-local.scm -p ${GUIX_PROFILE_LOCAL} \
# 		${PULL_EXTRA_OPTIONS}


channels/local.scm: local_checkouts

# channels/lock-local.scm: channels/local.scm
# 	$(call T, "channels/lock-local.scm (update local locks)")
# 	echo -e "(use-modules (guix channels))\n" > ./channels/lock-local-tmp.scm
# 	$(GUIX) time-machine -C ./channels/local.scm \
# 		-- describe -f channels >> ./channels/lock-local-tmp.scm
# 	mv ./channels/lock-local-tmp.scm ./channels/lock-local.scm

pi:
	# guix build --list-targets
	guix build --target=aarch64-linux-gnu  -f rpi.scm

help:
	@echo "GUIX=$(GUIX)"
	@echo "GUIX_LOCK=$(GUIX_LOCK)"
	@echo "GUIX_PROFILE=$(GUIX_PROFILE)"
	@echo "HOST=$(HOST)"
	@echo "PULL_EXTRA_OPTIONS=$(PULL_EXTRA=OPTIONS)"
	@echo "RDE_TARGET=$(RDE_TARGET)"
	@echo "ROOT_MOUNT_POINT=$(ROOT_MOUNT_POINT)"
	@echo "TANGLES=$(TANGLES)"
	@echo "T_START=$(T_START)"
	@echo "VERSION=$(VERSION)"
