OBJS = ast.cmo symtab.cmo check.cmo parser.cmo scanner.cmo lorax.cmo

lorax : $(OBJS)
	ocamlc -o lorax -g $(OBJS)

.PHONY : test
test : lorax testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c -g $<

%.cmi : %.mli
	ocamlc -c -g $<

.PHONY : clean
clean :
	rm -f lorax parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff *~

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
symtab.cmo: ast.cmo
symtab.cmx: ast.cmx
check.cmo: symtab.cmo
check.cmx: symtab.cmx
lorax.cmo: scanner.cmo parser.cmi ast.cmo symtab.cmo
lorax.cmx: scanner.cmx parser.cmx ast.cmx symtab.cmx
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
