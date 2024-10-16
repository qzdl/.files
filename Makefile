default: help
# To rebuild lock.scm use `make -B guix`

HOST ?= $(shell hostname)
RDE_TARGET ?= $(HOST)

# Also defined in .envrc to make proper guix version available project-wide
GUIX_PROFILE      := target/profiles/guix-local
GUIX_PROFILE_HEAD := $(shell basename $(GUIX_PROFILE))
GUIX_CHANNELS     := channels/$(GUIX_PROFILE_HEAD).scm
GUIX_LOCK         := $(GUIX_CHANNELS).lock
GUIX_LOCK_TMP     := $(GUIX_LOCK).tmp
GUIX              := ./pre-inst-env ${GUIX_PROFILE}/bin/guix

GUIX_HOME   := RDE_TARGET=$(HOST)-home ${GUIX} home
GUIX_SYSTEM := RDE_TARGET=$(HOST)-system ${GUIX} system

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

PULL_OPTIONS  :=
BUILD_OPTIONS := --cores=32 --max-jobs=32 --rounds=2

ROOT_MOUNT_POINT=/mnt

VERSION=latest
TANGLES=$(shell rg -o ':tangle .+' $(CONFIGO) | cut -d' ' -f2 | sed 's/:tangle //g' | grep -vE 'no|ident' | sort -n)

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

all:    check guix reconfigure/home reconfigure/system
reconfigure: all
home:   reconfigure/home
system: reconfigure/system
build: guix build/home build/system

sanity:
	guile \
		-L ${GUIX_PROFILE}/share/guile/site/3.0 \
		-L ./src -c '(use-modules (emacs))'

	${GUIX} repl \
		-L ${GUIX_PROFILE}/share/guile/site/3.0 \
		-L src \
		src/sanity.scm

check: tangle inputs lint-elisp

tangle:
	$(EBATCH) \
	  -f package-initialize ${CONFIGO} \
	  --eval '(setq buffer-read-only t)' \
	  -f org-babel-tangle 1>2
inputs:
	$(call T, HEAD		$(shell git rev-parse HEAD))
	git show HEAD --stat --oneline
	$(call T, PROFILE (GUIX_PROFILE=$(GUIX_PROFILE)))
	guix describe -p $(GUIX_PROFILE)
	$(call T, CHANNELS (LOCAL))
	git submodule status
	git submodule foreach --recursive \
		git show HEAD --stat --oneline
	$(call T, PROFILE (LOCAL))
	guix describe -p target/profiles/guix-local
	$(call T, HOME)
	guix home describe
	$(call T, SYSTEM)
	guix system describe
	$(call T, TANGLE)
	for file in $(TANGLES) ; do \
		cd src &>/dev/null ;\
                echo $$(guix hash -x $$file) $$file ;\
	done

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

graph:
	${GUIX_HOME} \
		shepherd-graph ${CONFIGS}
		> docs/shepherd.dot
	${GUIX_HOME} \
		extension-graph ${CONFIGS} \
		> docs/extension.dot
	@echo xdot --filter=$(GRAPH) docs/extension.dot
	@echo xdot --filter=$(GRAPH) docs/shepherd.dot

search:
	$(GUIX_HOME) \
		search "cron" ${CONFIGS}

build/home:
	${GUIX_HOME} \
		build $(BUILD_OPTIONS) \
		${CONFIGS}
build/system:
	${GUIX_SYSTEM} \
		build $(BUILD_OPTIONS) \
		${CONFIGS}

container/home: guix
	$(GUIX_HOME) \
		container ${CONFIGS}
container/system:
	$(GUIX_SYSTEM) \
		container ${CONFIGS}
vm:
	${GUIX_SYSTEM} \
		vm ${CONFIGS}

reconfigure/home:
	$(GUIX_HOME) \
		reconfigure ${CONFIGS}
reconfigure/system:
	$(GUIX_SYSTEM) \
		reconfigure ${CONFIGS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

system/init: guix
	$(GUIX_SYSTEM) \
		init ${CONFIGS} ${ROOT_MOUNT_POINT}

pi:
	# guix build --list-targets
	guix build --target=aarch64-linux-gnu  -f rpi.scm

image/build: guix
	$(GUIX_SYSTEM) \
		image --image-type=iso9660 \
		${CONFIGS}

target/rde-live.iso: guix target
	$(GUIX_SYSTEM) \
		image --image-type=iso9660 \
		${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso

target:
	@mkdir -p target

target/release:
	@mkdir -p target/release

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

clean-target:
	rm -rf ./target

clean: clean-target

mixxx:
	ln -s $$HOME/git/sys/mixxx-Old-School-Skin \
		$$HOME/.mixxx/skins/mixxx-Old-School-Skin

# Profiles
# `make -B guix` :: what happens?
#
#   0   guix
#   0.0   target/profiles                     (mkdir)
#   0.1   target/profiles/guix-time-marker    (locking-mechanism)
#   0.2   lock                                (pull-passthrough)
#   0.2.0   $(GUIX_LOCK)		      ()
#   0.2.0.0   $(GUIX_CHANNELS)		      ()
#   0.2.0.0.1   channels/guix-local.scm       (map git-pull submodules)

guix:  target/profiles target/profiles/guix-time-marker lock

target/profiles:
	$(call T, target/profiles (init profiles dir))
	@mkdir -p $@

# Store-items (i.e. profiles) don't have useful mtime, so we rely on
# lockfiles to prevent unnecessary rebuilds
target/profiles/guix-time-marker:
	$(call T, target/profiles/guix-time-marker (create update-lock))
	touch $@



channels/guix-local.scm:
	$(call T, channels/guix-local.scm (check 0/1 local-channel))
	@$(GIT) submodule status
	$(call T, channels/guix-local.scm (update local-channel))
	$(GIT) submodule foreach --recursive git pull --all
	$(call T, channels/guix-local.scm (check 1/1 local-channel))
	@$(GIT) submodule status

$(GUIX_LOCK): $(GUIX_CHANNELS)
	$(call T, $(GUIX_LOCK) (pull channel))
	guix time-machine -C $(GUIX_CHANNELS) -- pull -C $(GUIX_CHANNELS)
	$(call T, $(GUIX_LOCK) (start locking))
	echo -e "(use-modules (guix channels))\n" \
		> $(GUIX_LOCK_TMP)

	guix time-machine -C $(GUIX_CHANNELS) -- describe -f channels \
		>> $(GUIX_LOCK_TMP)
	$(call T, $(GUIX_LOCK) (finish locking))
	mv $(GUIX_LOCK_TMP) \
	   $(GUIX_LOCK)

lock: $(GUIX_LOCK)
pull: $(GUIX_CHANNELS)
