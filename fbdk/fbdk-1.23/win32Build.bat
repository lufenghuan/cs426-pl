echo let version = "1.1" > src\version.ml
echo let date = "unknown" >> src\version.ml
ocamlyacc -v src\Fb\fbparser.mly
ocamllex src\Fb\fblexer.mll
ocamlyacc -v src\FbSR\fbsrparser.mly
ocamllex src\FbSR\fbsrlexer.mll
ocamlyacc -v src\TFbSRX\tfbsrxparser.mly
ocamllex src\TFbSRX\tfbsrxlexer.mll
ocamlyacc -v src\AFbV\afbvparser.mly
ocamllex src\AFbV\afbvlexer.mll
ocamlyacc -v src\BOOL\boolparser.mly
ocamllex src\BOOL\boollexer.mll
ocamlyacc -v src\FbExt\fbextparser.mly
ocamllex src\FbExt\fbextlexer.mll
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\version.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\fbdk.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\application.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbtype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fblexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fbinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\Fb\fb.ml
ocamlc -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt -o src\Fb\fb.opt src\version.cmo src\fbdk.cmo src\application.cmo src\Fb\fbast.cmo src\Fb\fbparser.cmo src\Fb\fbtype.cmo src\Fb\fbpp.cmo src\Fb\fblexer.cmo src\Fb\fbinterp.cmo src\Fb\fb.cmo
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrtype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrlexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsrinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbSR\fbsr.ml
ocamlc -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt -o src\FbSR\fbsr.opt src\version.cmo src\fbdk.cmo src\application.cmo src\FbSR\fbsrast.cmo src\FbSR\fbsrparser.cmo src\FbSR\fbsrtype.cmo src\FbSR\fbsrpp.cmo src\FbSR\fbsrlexer.cmo src\FbSR\fbsrinterp.cmo src\FbSR\fbsr.cmo
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\booltype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boollexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\boolinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\BOOL\bool.ml
ocamlc -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt -o src\BOOL\bool.opt src\version.cmo src\fbdk.cmo src\application.cmo src\BOOL\boolast.cmo src\BOOL\boolparser.cmo src\BOOL\booltype.cmo src\BOOL\boolpp.cmo src\BOOL\boollexer.cmo src\BOOL\boolinterp.cmo src\BOOL\bool.cmo
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbexttype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextlexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbextinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\FbExt\fbext.ml
ocamlc -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt -o src\FbExt\fbext.opt src\version.cmo src\fbdk.cmo src\application.cmo src\FbExt\fbextast.cmo src\FbExt\fbextparser.cmo src\FbExt\fbexttype.cmo src\FbExt\fbextpp.cmo src\FbExt\fbextlexer.cmo src\FbExt\fbextinterp.cmo src\FbExt\fbext.cmo
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxtype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxlexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrxinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\TFbSRX\tfbsrx.ml
ocamlc -I src -I src\Fb -I src\TFbSRX -I src\BOOL -I src\TTFbSRXX -I src\AFbV -I src\FbExt -o src\TFbSRX\tfbsrx.opt src\version.cmo src\fbdk.cmo src\application.cmo src\TFbSRX\tfbsrxast.cmo src\TFbSRX\tfbsrxparser.cmo src\TFbSRX\tfbsrxtype.cmo src\TFbSRX\tfbsrxpp.cmo src\TFbSRX\tfbsrxlexer.cmo src\TFbSRX\tfbsrxinterp.cmo src\TFbSRX\tfbsrx.cmo
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvast.ml
ocamlc -c -g -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvparser.mli
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvparser.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvtype.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvpp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvlexer.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbvinterp.ml
ocamlc -c -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt src\AFbV\afbv.ml
ocamlc -I src -I src\Fb -I src\FbSR -I src\BOOL -I src\TFbSRX -I src\AFbV -I src\FbExt -o src\AFbV\afbv.opt src\version.cmo src\fbdk.cmo src\application.cmo src\AFbV\afbvast.cmo src\AFbV\afbvparser.cmo src\AFbV\afbvtype.cmo src\AFbV\afbvpp.cmo src\AFbV\afbvlexer.cmo src\AFbV\afbvinterp.cmo src\AFbV\afbv.cmo
copy /Y src\Fb\fb.opt .\Fb
copy /Y src\FbSR\fbsr.opt .\FbSR
copy /Y src\BOOL\bool.opt .\BOOL
copy /Y src\FbExt\fbext.opt .\FbExt
copy /Y src\TFbSRX\tfbsrx.opt .\TFbSRX
copy /Y src\AFbV\afbv.opt .\AFbV

