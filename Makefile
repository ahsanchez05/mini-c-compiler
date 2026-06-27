.PHONY: help build-ocaml-image build-x86-image build-images dev-shell mini-shell clean

CONTAINER_RUNTIME ?= docker

# Development image for OCaml
IMAGE_DEV_SHELL ?= mini-c-compiler-ocaml:latest
# x86 image used only to assemble and run generated x86 assembly 
IMAGE_X86_SHELL ?= mini-c-compiler-x86:latest

WORKDIR := /mini-c-compiler

DOCKER_DIR := docker

build-ocaml-image:
	$(CONTAINER_RUNTIME) build \
		-t $(IMAGE_DEV_SHELL) \
		-f $(DOCKER_DIR)/Dockerfile.ocaml .

build-x86-image:
	$(CONTAINER_RUNTIME) build \
		--platform linux/amd64 \
		-t $(IMAGE_X86_SHELL) \
		-f $(DOCKER_DIR)/Dockerfile.x86 .

build-images: build-ocaml-image build-x86-image

# OCaml development shell
dev-shell:
	$(CONTAINER_RUNTIME) run --rm -it \
		--name ocaml-dev-shell \
		--hostname ocaml-dev-shell \
		-v "$$(pwd -P):$(WORKDIR)" \
		-w $(WORKDIR) \
		$(IMAGE_DEV_SHELL) bash

# x86_64 shell for assembling/running generated x86 assembly
mini-shell:
	$(CONTAINER_RUNTIME) run --rm -it \
		--name x86-shell \
		--hostname x86-shell \
		--platform linux/amd64 \
		--env DEBIAN_FRONTEND=noninteractive \
		-v "$$(pwd -P):$(WORKDIR)" \
		-w $(WORKDIR) \
		$(IMAGE_X86_SHELL) bash

help:
	@echo ""
	@echo "Available commands"
	@echo ""
	@echo "  make build-images"
	@echo "  make build-ocaml-image"
	@echo "  make build-x86-image"
	@echo "  make dev-shell"
	@echo "  make mini-shell"
	@echo "  make clean"
	@echo "  make clean-images"
	@echo ""

clean:
	rm -rf _build

clean_images:
	-$(CONTAINER_RUNTIME) rmi -f $(IMAGE_DEV_SHELL) $(IMAGE_X86_SHELL)
