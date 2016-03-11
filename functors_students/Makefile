c_p = collections/

all:
	ocamlc -I collections/ -o functors.out $(c_p)Collection.mli $(c_p)Lists.ml ColUtils.mli ColUtils.ml ColUtilsTest.ml

clean:
	rm *.cmo *.cmi *.out $(c_p)*.cmo $(c_p)*.cmi
