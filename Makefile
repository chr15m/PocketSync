# dev targets

.PHONY: watch clean

watch:
	npx shadow-cljs watch app 

repl:
	npx shadow-cljs cljs-repl app

clean:
	rm -rf build/*
