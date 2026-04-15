.PHONY: all server cli clean

STATIC_DIR := frontend/static
APP_JS := $(STATIC_DIR)/app.js

all: server

$(APP_JS): frontend/src/Main.elm
	cd frontend && elm make src/Main.elm --output=static/app.js

$(STATIC_DIR):
	mkdir -p $(STATIC_DIR)

site: $(APP_JS)

server: $(APP_JS)
	cabal run server -O0 -j

cli:
	cabal run cli -O0 -j

release:
	cd frontend && elm make src/Main.elm --output=static/app.js --optimize
	cabal run server -j -O2
	
clean:
	cabal clean
	rm -rf $(STATIC_DIR)
