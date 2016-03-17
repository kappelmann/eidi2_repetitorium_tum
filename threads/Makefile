tree:
	ocamlc -o threadedTree.out -thread -I +threads unix.cma threads.cma ThreadedTree.mli ThreadedTree.ml ThreadedTreeTest.ml

future:
	ocamlc -o future.out -thread -I +threads unix.cma threads.cma Future.mli Future.ml FutureTest.ml

wrapselect:
	ocamlc -o wrapselect.out -thread -I +threads unix.cma threads.cma WrapSelect.mli WrapSelect.ml

evenMore:
	ocamlc -o evenMore.out -thread -I +threads unix.cma threads.cma EvenMoreThreads.mli EvenMoreThreads.ml

clean:
	rm *.cmo *.cmi *.out
