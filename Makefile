all: install test

install: package.json
	npm install --no-package-lock --silent

test:
	./node_modules/.bin/elm-test

FORCE:
