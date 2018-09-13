all: test

install: package.json
	npm install

test: install
	./node_modules/.bin/elm-test

FORCE:
