# Every emacs invocation sandboxes user-emacs-directory: neither -Q nor
# --batch relocates it, so stray runs would write into the real ~/.emacs.d
SANDBOX = $(CURDIR)/.sandbox
ELPA_DIR = $(CURDIR)/.elpa

EMACS_BATCH = emacs -Q --batch --init-directory $(SANDBOX) \
	--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	--eval "(package-initialize)"

EMACS_SANDBOX = emacs -Q --init-directory $(SANDBOX) \
	--eval "(setq package-user-dir \"$(ELPA_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(package-initialize)"

.PHONY: help deps sandbox test test-e2e test-all lint check-autoloads check-compile compile clean

help:
	@echo "Available commands:"
	@echo "  make deps            Install dependencies"
	@echo "  make sandbox         Launch emacs -Q with org-edit-indirect-mode in Org buffers"
	@echo "  make test            Run unit tests"
	@echo "  make test-e2e        Type the E2E cases into Org files in batch Emacs"
	@echo "  make test-all        Run all tests"
	@echo "  make lint            package-lint and checkdoc the package file"
	@echo "  make compile         Byte-compile the package"
	@echo "  make check-autoloads Generate and load autoloads"
	@echo "  make check-compile   Check for clean byte-compilation"
	@echo "  make clean           Remove compiled files and the sandbox"

$(ELPA_DIR):
	@echo "Installing dependencies..."
	$(EMACS_BATCH) \
	--eval "(package-refresh-contents)" \
	--eval "(package-install 'edit-indirect)" \
	--eval "(package-install 'buttercup)" \
	--eval "(package-install 'package-lint)"

deps: $(ELPA_DIR)

sandbox: $(ELPA_DIR)
	$(EMACS_SANDBOX) --directory . \
	--eval "(require 'org-edit-indirect)" \
	--eval "(add-hook 'org-mode-hook #'org-edit-indirect-mode)" \
	--eval "(message \"org-edit-indirect sandbox: C-c ' on any element of an Org buffer\")"

test: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(setq buttercup-stack-frame-style 'omit)" \
	-l test/org-edit-indirect-tests.el \
	--funcall buttercup-run

# A prompt the keys do not answer reads stdin; /dev/null fails it fast
test-e2e: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . -l test/org-edit-indirect-e2e.el < /dev/null

test-all: test test-e2e

lint: $(ELPA_DIR)
	$(EMACS_BATCH) --directory . \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit org-edit-indirect.el
	$(EMACS_BATCH) \
	--eval "(require 'checkdoc)" \
	--eval "(setq checkdoc-verb-check-experimental-flag t)" \
	--eval "(with-current-buffer (find-file-noselect \"org-edit-indirect.el\") \
	           (check-parens) \
	           (checkdoc-current-buffer t))" \
	--eval "(when-let* ((buf (get-buffer \"*Style Warnings*\")) \
	                    (text (with-current-buffer buf (buffer-string))) \
	                    (_ (string-match-p \"[.]el:[0-9]+:\" text))) \
	           (princ text) \
	           (kill-emacs 1))"

check-autoloads:
	@echo "Generating and loading autoloads..."
	rm -f org-edit-indirect-autoloads.el
	$(EMACS_BATCH) \
	--eval "(require 'loaddefs-gen)" \
	--eval "(loaddefs-generate \"$(CURDIR)\" (expand-file-name \"org-edit-indirect-autoloads.el\" \"$(CURDIR)\"))" \
	--eval "(load (expand-file-name \"org-edit-indirect-autoloads.el\" \"$(CURDIR)\") nil 'nomessage)"

check-compile: $(ELPA_DIR) check-autoloads
	@echo "Checking byte-compilation..."
	$(EMACS_BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(unless (byte-compile-file \"org-edit-indirect.el\") (kill-emacs 1))"
	rm -f org-edit-indirect.elc

compile: $(ELPA_DIR)
	@echo "Byte-compiling package files..."
	$(EMACS_BATCH) \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(byte-compile-file \"org-edit-indirect.el\")"

clean:
	@echo "Cleaning compiled files..."
	rm -f *.elc test/*.elc
	rm -rf $(ELPA_DIR) $(SANDBOX)
