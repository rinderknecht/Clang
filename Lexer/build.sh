#!/bin/sh
set -x
ocamllex.opt Lexer.mll
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Error.mli
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Token.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Error.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Token.ml
ocamlfind ocamlc -strict-sequence -w +A-48-4 -c Lexer.mli
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c LexerMain.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c Lexer.ml
ocamlfind ocamlopt -strict-sequence -w +A-48-4 -c LexerMain.ml
ocamlfind ocamlopt -o LexerMain.opt Error.cmx Token.cmx Lexer.cmx LexerMain.cmx
