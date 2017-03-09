all:
	ocamlc Collection.mli Lists.ml Tree.mli BinTree.ml

lists:
	ocamlc -o lists.out Collection.mli Lists.ml ListTests.ml

tree: 
	ocamlc -o tree.out Collection.mli Tree.mli BinTree.ml BinTreeTest.ml

clean:
	rm *.cmo *.cmi *.out
