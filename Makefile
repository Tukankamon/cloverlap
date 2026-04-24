.PHONY: all server cli clean

STATIC_DIR := frontend/static
APP_JS := $(STATIC_DIR)/app.js

all: server

ELM_SOURCES := $(shell find frontend/src -name "*.elm")

$(APP_JS): $(ELM_SOURCES)
	cd frontend && elm make src/Main.elm --output=static/app.js

$(STATIC_DIR):
	mkdir -p $(STATIC_DIR)

site: $(APP_JS)

server: $(APP_JS)
	cabal run server -O0 -j

cli:
	cabal run cli -O0 -j

site-release: frontend/src/Main.elm
	# Only a 2.5% difference in the size of app.js
	cd frontend && elm make src/Main.elm --output=static/app.js --optimize

release: site-release
	cabal run server -j -O2
	
clean:
	cabal clean
	rm -rf $(STATIC_DIR)
