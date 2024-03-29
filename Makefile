build: $(shell find src) public/* node_modules
	mkdir -p build
	npx shadow-cljs release app
	rsync -aLz --exclude js --exclude '.*.swp' public/ build
	touch build

node_modules: package.json
	pnpm i --no-lockfile --shamefully-hoist

.PHONY: watch clean

watch: node_modules
	npm run watch

repl: node_modules
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build

