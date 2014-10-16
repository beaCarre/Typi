all:
	ocp-build
	rm -f *.old
	mv _obuild/main_js/main_js.byte .
	js_of_ocaml main_js.byte
	mv main_js.js typi_web
clean:
	rm -f -R _obuild *.~ *.js *.asm
