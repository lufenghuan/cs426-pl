module Language = struct
  
  let name = "FbExt"
  module Parser = Fbextparser
  module Lexer = Fbextlexer
  module Ast = Fbextast
  module Pp = Fbextpp
  module Interpreter = Fbextinterp
	module Typechecker = Fbexttype

end;;

module Application = Application.Make(Language);;

Application.main ();;
