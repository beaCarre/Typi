all:
	ocp-build root
	ocp-build
	rm -f *.old
	mv _obuild/typi_js/typi_js.byte .
	js_of_ocaml typi_js.byte
	mv typi_js.js typi_web
clean:
	rm -f -R _obuild *.~ *.js *.asm *.byte *.root *~ ./typi_web/typi_js.js ./typi_web/*~
