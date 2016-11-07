test: przelewanka.cmo tests.cmo
	ocamlc -o test przelewanka.cmo tests.cmo

przelewanka.cmo: przelewanka.ml
	ocamlc -c przelewanka.ml

tests.cmo: tests.ml przelewanka.cmo
	ocamlc -c tests.ml

clean:
	rm -f *.cmi *.cmo ./test
