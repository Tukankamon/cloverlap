.PHONY: all build-backend build-frontend dev clean

all: server

site: $(STATIC_DIR)
	cd frontend && elm make src/Main.elm --output=static/app.js

server: site
	cabal run server

cli:
	cabal run cli

clean:
	cabal clean
	rm -rf frontend/static
