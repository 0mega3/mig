#############################################################################
#
# Makefile to build all.
#
# copyright 2025 by Denis Khorkin
# license   BSD-3-Clause
# author    Denis Khorkin <denkhorkin@gmail.com>
#############################################################################

REPO_CVR = https://github.com/0mega3/mig/raw/main/doc/src/assets/cover.png
REPO_LNK = ![cover]($(REPO_CVR))

DOC_LNK = ![cover](assets/cover.png)


# Build
#
## Doc
.PHONY: _sync-readme doc	# .PHONY says that these targets are an action

_sync-readme:	# Hidden target (starts with _) - cannot be called directly
	@echo "Syncing README..."
	@mkdir -p doc/src/extras/ && \
	cp README.md doc/src/extras/README.md && \
	perl -wpi -e 's|\Q$(REPO_LNK)\E|$(DOC_LNK)|g' \
		doc/src/extras/README.md > /dev/null 2>&1 && \
	sed -i -e '/[Aa][Pp][Ii] [Dd]ocumentation/d' -e '/Start the frontend/,+2d' \
		doc/src/extras/README.md > /dev/null 2>&1
	@echo "README synced"

doc: _sync-readme
	@echo "Building docs..."
	@rebar3 ex_doc			#backend
	@jsdoc -c conf.json		#frontend
	@echo "Docs built"


# Clean
#
.PHONY: clean-doc clean-all

clean-doc:
	@echo "Removing docs..."
	@rm -rf apps/mig/doc 2>/dev/null || true
	@rm doc/src/extras/README.md 2>/dev/null || true
	@echo "Docs removed"

clean-all: clean-doc
