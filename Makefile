# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

ifeq "$(wildcard distrib/)" ""

# We're in a bindist

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

.PHONY: install
install:
	$(MAKE) -r --no-print-directory -f ghc.mk install BINDIST=YES NO_INCLUDE_DEPS=YES

.PHONY: show
show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

else

default : all
	@:

# For help, type 'make help'
.PHONY: help
help :
	@cat MAKEHELP

ifneq "$(findstring clean,$(MAKECMDGOALS))" ""
-include mk/config.mk
else
include mk/config.mk
ifeq "$(ProjectVersion)" ""
$(error Please run ./configure first)
endif
endif

include mk/custom-settings.mk

# No need to update makefiles for these targets:
REALGOALS=$(filter-out bootstrapping-files framework-pkg clean clean_% distclean maintainer-clean show help,$(MAKECMDGOALS))

# On Solaris, the builtin test doesn't support -nt, you have to use
# /usr/bin/test.  On MSYS, however, /usr/bin/test does not exist.  How
# nice.
TEST=$(if $(wildcard /usr/bin/test),/usr/bin/test,test)

# configure touches certain files even if they haven't changed.  This
# can mean a lot of unnecessary recompilation after a re-configure, so
# here we cache the old versions of these files so we can restore the
# timestamps.
#
define check-configure-file
# $1 = file
if $(TEST) ! -f $1.old; then \
  echo "backing up $1"; \
  cp $1 $1.old; \
  touch -r $1 $1.old; \
else \
  if $(TEST) $1 -nt $1.old; then \
    if cmp $1 $1.old; then \
       echo "$1 has been touched, but has not changed"; \
       touch -r $1.old $1; \
    else \
       echo "$1 has changed"; \
       cp $1 $1.old; \
       touch -r $1 $1.old; \
    fi \
  fi \
fi
endef

# NB. not the same as saying '%: ...', which doesn't do the right thing:
# it does nothing if we specify a target that already exists.
.PHONY: $(REALGOALS)
$(REALGOALS) all:
	@$(call check-configure-file,mk/config.mk)
	@$(call check-configure-file,mk/project.mk)
	@$(call check-configure-file,compiler/ghc.cabal)

	@echo "===--- updating makefiles phase 0"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=0 just-makefiles
ifneq "$(OMIT_PHASE_1)" "YES"
	@echo "===--- updating makefiles phase 1"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=1 just-makefiles
endif
ifneq "$(OMIT_PHASE_2)" "YES"
	@echo "===--- updating makefiles phase 2"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=2 just-makefiles
endif
ifneq "$(OMIT_PHASE_3)" "YES"
	@echo "===--- updating makefiles phase 3"
	$(MAKE) -r --no-print-directory -f ghc.mk phase=3 just-makefiles
endif
	@echo "===--- finished updating makefiles"
	$(MAKE) -r --no-print-directory -f ghc.mk $@

binary-dist:
ifeq "$(mingw32_TARGET_OS)" "1"
	$(MAKE) -r --no-print-directory -f ghc.mk windows-binary-dist
	$(MAKE) -r --no-print-directory -f ghc.mk windows-installer
else
	rm -f bindist-list
	$(MAKE) -r --no-print-directory -f ghc.mk bindist BINDIST=YES
	$(MAKE) -r --no-print-directory -f ghc.mk binary-dist
endif

clean distclean maintainer-clean:
	$(MAKE) -r --no-print-directory -f ghc.mk $@ CLEANING=YES
	test ! -d testsuite || $(MAKE) -C testsuite $@

$(filter clean_%, $(MAKECMDGOALS)) : clean_% :
	$(MAKE) -r --no-print-directory -f ghc.mk $@ CLEANING=YES

bootstrapping-files show:
	$(MAKE) -r --no-print-directory -f ghc.mk $@

ifeq "$(darwin_TARGET_OS)" "1"
framework-pkg:
	$(MAKE) -C distrib/MacOS $@
endif

# If the user says 'make A B', then we don't want to invoke two
# instances of the rule above in parallel:
.NOTPARALLEL:

endif
