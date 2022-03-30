clean:
	rm -rf ./dist

build: clean
	mkdir -p dist
	cp public/* dist
	elm make src/Main.elm --output=dist/index.js

live: clean build
	npx elm-live src/Main.elm --dir=dist -- --output=dist/index.js
