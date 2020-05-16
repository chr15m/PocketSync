# dev targets

.PHONY: watch clean

build/index.html: src/poq/* package.json public/*
	npx shadow-cljs release app
	cp -v public/* build/
	touch build/index.html

watch:
	npx shadow-cljs watch app 

repl:
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build/*
