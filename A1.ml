type variable=X of int|Y|Z of int;;
type instruction=V of variable|Inc of variable|Dec of variable|Lab of int|Jnz of variable*instruction|Nop|Halt|Goto of instruction|Copy of variable*variable|Const of variable*int|Sum of variable*variable*variable|Minus of variable*variable*variable|Times of variable*variable*variable|Compare of variable*variable*variable|Factorial of variable*variable|CheckGoto of variable*variable*instruction|Gmac of string*(instruction list);;

let varHash=Hashtbl.create 1000;;
let macroHash=Hashtbl.create 100;;
let labHash=Hashtbl.create 1000;;
let rec listMake inst n=
if n=0 then []
else inst@(listMake inst (n-1));;
let factorial v varCount labelCount=
		let [V v1;V v2]=v in
				[	Copy (Z varCount,v2);
					Const (v1,1);
					Jnz (Z varCount,Lab labelCount);
					Goto (Lab (labelCount + 1));
					Lab labelCount;
					Times (v1,v1,Z varCount);
					Dec (Z varCount);
					Lab (labelCount + 1);
					Jnz (Z varCount,Lab labelCount)	] ;;
let checkGoto v varCount labelCount=
		let [V v1;V v2;l]=v in
				[	Compare (Z varCount,v1,v2);
					Jnz (Z varCount,Lab labelCount);
					Goto l;
					Lab labelCount	] ;;
let addMacro f s=
Hashtbl.add macroHash s f;;
addMacro factorial "fact";;
addMacro checkGoto "GotoIf";;
let macroFill varCount labelCount macro=match macro with
	(Goto l) -> 
				[	Inc (Z varCount);
					Jnz (Z varCount,l)	] |
	(Copy (v1,v2)) -> 
				[	Lab labelCount;
					Dec v1;
					Jnz (v1,Lab labelCount);
					Jnz (v2,Lab (labelCount + 1));
					Goto (Lab (labelCount + 2));
					Lab (labelCount + 1);
					Dec v2;
					Inc v1;
					Inc (Z varCount);
					Jnz (v2,Lab (labelCount + 1));
					Lab (labelCount + 3);Inc v2;
					Dec (Z varCount);
					Jnz (Z varCount,Lab (labelCount + 3));
					Lab (labelCount + 2)	] |
	(Const (v1,n)) -> 
				[	Lab labelCount;Dec v1;
					Jnz (v1,Lab labelCount)	] @ (listMake [Inc v1] n) |
	(Sum (v1,v2,v3)) -> 
				[	Copy (Z varCount,v2);
					Copy (Z (varCount + 1),v3);
					Jnz (Z (varCount + 1),Lab labelCount);
					Goto (Lab (labelCount + 1));
					Lab labelCount;
					Dec (Z (varCount + 1));
					Inc (Z varCount);
					Jnz (Z (varCount + 1),Lab labelCount);
					Lab (labelCount + 1);
					Copy (v1,Z varCount)	] |
	(Minus (v1,v2,v3)) -> 
				[	Compare (Z (varCount + 2),v2,v3);
					Jnz (Z (varCount + 2),Lab (labelCount + 2));
					Lab (labelCount + 3);
					Goto (Lab (labelCount + 3));
					Lab (labelCount + 2);
					Copy (Z varCount,v2);
					Copy (Z (varCount + 1),v3);
					Jnz (Z (varCount + 1),Lab labelCount);
					Goto (Lab (labelCount + 1));
					Lab labelCount;
					Dec (Z (varCount + 1));
					Dec (Z varCount);
					Jnz (Z (varCount + 1),Lab labelCount);
					Lab (labelCount + 1);
					Copy (v1,Z varCount)	] |
	(Times (v1,v2,v3)) -> 
				[	Copy (Z varCount,v2);
					Copy (Z (varCount + 1),v3);
					Const (v1,0);
					Jnz (Z (varCount + 1),Lab labelCount);
					Goto (Lab (labelCount + 1));
					Lab labelCount;
					Sum (v1,v1,Z varCount);
					Dec (Z (varCount + 1));
					Jnz (Z (varCount + 1),Lab labelCount);
					Lab (labelCount + 1)	] |
	Compare (v1,v2,v3) -> 
				[	Copy (Z varCount,v2);
					Copy (Z (varCount + 1),v3);
					Lab (labelCount + 3);
					Jnz (Z (varCount + 1),Lab labelCount);
					Const (v1,1);
					Goto (Lab (labelCount + 1));
					Lab (labelCount + 2);
					Dec (Z varCount);
					Dec (Z (varCount + 1));
					Goto (Lab (labelCount + 3));
					Lab labelCount;
					Jnz (Z varCount,Lab (labelCount + 2));
					Const (v1,0);
					Lab (labelCount + 1)] |
	Gmac (s,v) ->
					(Hashtbl.find macroHash s) v varCount labelCount |					
	_ -> [];;	

let initialize list=
	let rec expandCode list alterList vCount lCount instCount=
				if (list<>[]) then 
				match (List.hd list) with 
				Inc v -> if((Hashtbl.mem varHash v) != true) then Hashtbl.add varHash v 0;
				expandCode (List.tl list) (alterList@[Inc v]) vCount lCount (instCount+1)
				|  
				Dec v -> if ((Hashtbl.mem varHash v) != true) then Hashtbl.add varHash v 0;
				expandCode (List.tl list) (alterList@[Dec v]) vCount lCount (instCount+1)
				| 
				Jnz (v, l) -> if ((Hashtbl.mem varHash v) != true) then Hashtbl.add varHash v 0;
				expandCode (List.tl list) (alterList@[Jnz (v,l)]) vCount lCount (instCount+1)
				|  
				Lab v -> Hashtbl.add labHash (Lab v) (instCount);
				expandCode (List.tl list) (alterList@[Lab v]) vCount lCount (instCount+1)
				|
				Nop -> 
				expandCode (List.tl list) (alterList@[Nop]) vCount lCount (instCount+1)
				|
				Halt ->
				expandCode (List.tl list) (alterList@[Halt]) vCount lCount (instCount+1)
				|
				x -> 
				expandCode ((macroFill vCount lCount x)@(List.tl list)) alterList (vCount+4) (lCount+4) instCount
			else alterList 
			in
			expandCode list [] 100 100 0;;


let execute list=
	let rec compile wList pc=
		match (List.nth wList pc) with
		(Lab l)->compile wList (pc+1)
		|
		(Inc v)-> Hashtbl.replace varHash v ((Hashtbl.find varHash v)+1);
		compile wList (pc+1) 
		|
		(Dec v)-> if ((Hashtbl.find varHash v)>0) then Hashtbl.replace varHash v ((Hashtbl.find varHash v)-1);
		compile wList (pc+1) 
		|
		(Jnz (v,l)) -> if (((Hashtbl.mem varHash v) = false) || ((Hashtbl.find varHash v) = 0)) then compile wList (pc+1)
		else compile wList (Hashtbl.find labHash l)
		|
		(Nop)-> compile wList (pc+1)
		|
		(Halt)-> ()
		|
		_-> ()
	in
	compile (initialize list) 0;;

(* let code=[	
			Const (X 1,5);
			Gmac ("fact",[V Y;V (X 1)]);
			Halt
			];;
execute code;;
Hashtbl.find varHash (X 1);;
Hashtbl.find varHash (Y);; *)
