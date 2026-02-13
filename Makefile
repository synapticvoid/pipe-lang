
.DEFAULT_GOAL := help

.PHONY: build
build: ## Build the project
	zig build

.PHONY: test
test: ## Run tests
	zig build test

.PHONY: install
install: build ## Install pipe to /usr/local/bin/
	sudo cp zig-out/bin/pipe /usr/local/bin/

.PHONY: help
help:
	@awk 'BEGIN {FS = ":.*?## "; printf "\nUsage:\n  make \033[36m<target>\033[0m\n\n"} \
		/^[a-zA-Z_-]+:.*?## / { printf "  \033[36m%-28s\033[0m %s\n", $$1, $$2 }' \
		$(MAKEFILE_LIST)
