tree:
	ocamlc -o threadedTree.out -thread -I +threads unix.cma threads.cma ThreadedTree.mli ThreadedTree.ml ThreadedTreeTest.ml

future:
	ocamlc -o future.out -thread -I +threads unix.cma threads.cma Future.mli Future.ml FutureTest.ml

clean:
	rm *.cmo *.cmi *.out
