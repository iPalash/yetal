{
	#use "A1.ml"
	let code=ref []
	let gVar=ref Y
	let gLab=ref (Lab 0)
	let gArgs=ref []
	let toVar x=
		if x.[0]='x' then (X (int_of_string(String.sub x 2 ((String.length x)-2))))
		else 
		if x.[0]='y' then Y
		else failwith "Invalid Variable"
}
let digit=['0'-'9']
let var_regex="x_"digit+|"z_"digit+|"y"
let lab_regex="L_"digit+|"E_"digit+
let fun_regex=['a'-'z''A'-'Z']*
let ws_regex=[' ''\t']*
rule getVar=parse 
	| " "{
		getVar lexbuf
	}
	| "x_"(digit+ as num){
		gVar:=X (int_of_string(num))
	}
	| "z_"(digit+ as num){
		gVar:=Z (int_of_string(num))
	}
	| "y"{
		gVar:=Y
	}
	| _ {
		failwith "Invalid Variable"
	}
and getArgs=parse
	| [' '','] {
		getArgs lexbuf
	}
	| "x_"(digit+ as num){
		gArgs:=((!gArgs)@[V (X (int_of_string(num)))]);
		getArgs lexbuf
	}
	| "Y"{
		gArgs:=((!gArgs)@[V Y]);
		getArgs lexbuf
	}
	| "L_"(digit+ as num){
		gArgs:=((!gArgs)@[Lab (int_of_string(num))]);
		getArgs lexbuf
	}
	| "E_"(digit+ as num){
		gArgs:=((!gArgs)@[Lab (int_of_string(num)+1000)]);
		getArgs lexbuf
	}
	| ')'{}
	| _{
		failwith "Invalid Arguments"
	}
and getLab=parse
	| " "{
		getVar lexbuf
	}
	| "L_"(digit+ as num){
		gLab:=Lab (int_of_string(num))
	}
	| _ {
		failwith "Invalid Labels"
	}
and syntactify= parse 
	| [' ''\t''\n']{
		syntactify lexbuf
	}
	| "INC"
		{
			getVar lexbuf;
			code:=((Inc (!gVar))::(!code));
			syntactify lexbuf
		}
	| "DEC"
		{
			getVar lexbuf;
			code:=((Dec (!gVar))::(!code));
			syntactify lexbuf
		}
	| "NOP"
		{
			code:=((Nop)::(!code));
			syntactify lexbuf
		}
	| "HALT"
		{
			code:=((Halt)::(!code));
			syntactify lexbuf
		}
	| "JNZ"
		{
			getVar lexbuf;
			getLab lexbuf;
			code:=((Jnz ((!gVar),(!gLab)))::(!code));
			syntactify lexbuf
		}
	| "GOTO"
		{
			getLab lexbuf;
			code:=((Goto (!gLab))::(!code));
			syntactify lexbuf
		}
	| (var_regex as var1)ws_regex":="(var_regex as var2){
		code:=((Copy ((toVar(var1)),(toVar(var2))))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="(digit+ as var2){
		code:=((Const ((toVar(var1)),int_of_string(var2)))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="(var_regex as var2)ws_regex"+"ws_regex(var_regex as var3){
		code:=((Sum ((toVar(var1)),(toVar(var2)),(toVar(var3))))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="ws_regex(var_regex as var2)ws_regex"-"ws_regex(var_regex as var3){
		code:=((Minus ((toVar(var1)),(toVar(var2)),(toVar(var3))))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="ws_regex(var_regex as var2)ws_regex"*"ws_regex(var_regex as var3){
		code:=((Times ((toVar(var1)),(toVar(var2)),(toVar(var3))))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="ws_regex(var_regex as var2)ws_regex">="ws_regex(var_regex as var3){
		code:=((Compare ((toVar(var1)),(toVar(var2)),(toVar(var3))))::(!code));
		syntactify lexbuf
	}
	| (var_regex as var1)ws_regex":="ws_regex(fun_regex as func)"("{
		getArgs lexbuf;
		gArgs:=((V (toVar(var1)))::(!gArgs));
		code:=((Gmac(func,(!gArgs)))::(!code));
		gArgs:=[];
		syntactify lexbuf
	}
	| eof {	}
	| _ {
		getVar lexbuf
	}
{
	let lexbuf= Lexing.from_channel stdin in 
		syntactify lexbuf;
	execute (List.rev (!code));
	print_endline (string_of_int(Hashtbl.find varHash (Y)))
}
