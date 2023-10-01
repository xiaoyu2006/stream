LISP ?= sbcl

# Load the system into REPL
load:
	$(LISP) --eval "(push \"./\" asdf:*central-registry*)" \
			--eval "(ql:quickload :stream)" \
			--eval "(in-package :stream)"

# Test the system
test:
	$(LISP) --eval "(push \"./\" asdf:*central-registry*)" \
			--eval "(ql:quickload :stream)" \
			--eval "(asdf:test-system :stream)" \
			--non-interactive
