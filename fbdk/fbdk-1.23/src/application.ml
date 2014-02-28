module type S =
sig
	val main: unit -> unit
end

module Make(Lang: Fbdk.LANGUAGE) =
struct
	
	let toplevel_loop typechecking_enabled show_types =
		Printf.printf "\t%s version %s\t"
			Lang.name Version.version;
		Printf.printf "\t(typechecker %s)\n\n"
			(if typechecking_enabled then "enabled" else "disabled");
		flush stdout;
		while true do
			Printf.printf "# ";
			flush stdout;
			let lexbuf = Lexing.from_channel stdin in
			let ast = Lang.Parser.main Lang.Lexer.token lexbuf in
			(
				try
					if typechecking_enabled then
						let exprtype = Lang.Typechecker.typecheck ast in
						if show_types then
							Printf.printf " : %s\n" (Lang.Pp.pp_type exprtype "    ")
						else
							()
					else
						()
				with Fbdk.TypecheckerNotImplementedException -> ()
			)
			;
			let result = Lang.Interpreter.eval ast in
			Printf.printf "==> %s\n" (Lang.Pp.pp result "    ");
			flush stdout
		done
	
	let run_file filename =
		let fin = open_in filename in
		let lexbuf = Lexing.from_channel fin in
		let ast = Lang.Parser.main Lang.Lexer.token lexbuf in
		let result = Lang.Interpreter.eval ast in
		Printf.printf "%s\n" (Lang.Pp.pretty_print result);
		flush stdout
	
	let print_version () =
		Printf.printf "%s version %s\nBuild Date: %s\n"
			Lang.name Version.version Version.date
	
	let main () =
		let filename = ref "" in
		let toplevel = ref true in
		let version = ref false in
		let no_typechecking =
			ref (not Lang.Typechecker.typecheck_default_enabled) in
		let no_type_display = ref false in
		Arg.parse
			[("--version",
				Arg.Set(version),
				"show version information");
		  ("--typecheck",
			  Arg.Clear(no_typechecking),
				"enable typechecking");
			("--no-typecheck",
				Arg.Set(no_typechecking),
				"disable typechecking");
			("--hide-types",
			  Arg.Set(no_type_display),
				"disable displaying of types")]
			(function fname ->
						filename := fname;
						version := false;
						toplevel := false)
			("Usage: " ^
				Lang.name ^
				" [ options ] [ filename ]\noptions:");
		
		if !version then
			print_version ()
		else if !toplevel then
			toplevel_loop (not (!no_typechecking)) (not (!no_type_display))
		else
			run_file !filename
	
end
