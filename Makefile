OCAMLC ?= ocamlc
BUILD_DIR := _build
SITEGEN := $(BUILD_DIR)/sitegen

.PHONY: build clean serve

build: $(SITEGEN)
	rm -rf dist
	mkdir -p dist/assets
	cp -R assets/. dist/assets/
	$(SITEGEN)

$(SITEGEN): src/sitegen.ml
	mkdir -p $(BUILD_DIR)
	cd $(BUILD_DIR) && $(OCAMLC) -I +unix -c -o sitegen.cmo ../src/sitegen.ml
	$(OCAMLC) -I +unix unix.cma -o $(SITEGEN) $(BUILD_DIR)/sitegen.cmo

serve: build
	cd dist && python3 -m http.server 8000

clean:
	rm -rf $(BUILD_DIR) dist
