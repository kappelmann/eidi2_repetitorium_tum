c_p = collections/
m_p = simpleMap/

collection:
	ocamlc -I $(c_p) -o collection.out $(c_p)Collection.mli $(c_p)Lists.ml ColUtils.mli ColUtils.ml ColUtilsTest.ml

map: 
	ocamlc -I $(m_p) -o map.out $(m_p)SimpleMap.mli $(m_p)SimpleMaps.ml EnhancedMap.mli EnhancedMap.ml


clean:
	rm *.cmo *.cmi *.out $(c_p)*.cmo $(c_p)*.cmi $(m_p)*.cmo $(m_p)*.cmi
