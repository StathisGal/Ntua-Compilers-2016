FLAGS = -v
BUILD-DIR = ./build
OCAMLOPT=/home/$(USER)/.opam/4.03.0/bin/ocamlopt
OCAMLC = $(OCAMLOPT) -c
OCAMLCI = $(OCAMLOPT) -i
LLVM = /home/$(USER)/.opam/4.03.0/lib/llvm


all: parser lexer types ast symbol scopes exp_code_gen code_gen compiler
	$(OCAMLOPT) -cc g++ -ccopt -L/usr/lib/llvm-3.5/lib -I $(LLVM)  Error.cmx Hashcons.cmx Identifier.cmx Types.cmx Symbol.cmx llvm.cmxa llvm_analysis.cmxa str.cmxa TypeInference.cmx Symbtest.cmx  Ast.cmx Scopes.cmx Parser.cmx Lexer.cmx  ExpCodeGen.cmx Codegen.cmx Main.cmx -o compiler
	cp compiler ./Testcases
	cp Edsger.sh ./Testcases

lexer: parser Lexer.mll       # generates lexer.ml
	ocamllex Lexer.mll
	$(OCAMLC) Parser.mli
	$(OCAMLC) Lexer.ml

parser: Parser.mly ast  # generates parser.ml and parser.mli
	ocamlyacc -v $(FLAGS) Parser.mly
	$(OCAMLC) Parser.mli
	$(OCAMLC) Parser.ml


compiler: parser error ast scopes
	$(OCAMLC) Parser.mli
	$(OCAMLC) -I /home/yannis/.opam/4.03.0/lib/llvm Main.ml


symbtest : Symbtest.ml symbol
	$(OCAMLCI) Symbtest.ml > Symbtest.mli
	$(OCAMLC) Symbtest.mli
	$(OCAMLC) Symbtest.ml

scopes:  Scopes.ml symbol symbtest type_inference
	$(OCAMLCI)  Scopes.ml > Scopes.mli	
	$(OCAMLC) Scopes.mli
	$(OCAMLC) Scopes.ml

ast: Ast.mli Ast.ml types
	$(OCAMLC) Ast.mli
	$(OCAMLC) Ast.ml

types: Types.mli Types.ml
	$(OCAMLC) Types.mli
	$(OCAMLC) Types.ml

hashcons: Hashcons.mli Hashcons.ml
	$(OCAMLC) Hashcons.mli
	$(OCAMLC) Hashcons.ml

error: Error.ml Error.mli
	$(OCAMLC) Error.mli
	$(OCAMLC) Error.ml

identifier: hashcons Identifier.ml Identifier.mli
	$(OCAMLC) Identifier.mli
	$(OCAMLC) Identifier.ml


symbol: identifier error types  Symbol.ml
	$(OCAMLCI) Symbol.ml > Symbol.mli
	$(OCAMLC) Symbol.mli
	$(OCAMLC) Symbol.ml

type_inference : TypeInference.ml symbol
	$(OCAMLC) TypeInference.ml

exp_code_gen: ExpCodeGen.ml
	$(OCAMLC) -I /home/yannis/.opam/4.03.0/lib/llvm ExpCodeGen.ml

code_gen: Codegen.ml
	$(OCAMLC) -I /home/yannis/.opam/4.03.0/lib/llvm Codegen.ml

clean: 
	$(RM) *.cmo *.cmx *.o *.cmi Parser.mli  Parser.ml Parser.output Lexer.ml 

distclean: clean
	$(RM) compiler ./Testcases/compiler ./Testcases/Edsger.sh

