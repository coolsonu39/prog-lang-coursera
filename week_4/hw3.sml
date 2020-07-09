(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*1*) 
fun only_capitals (xs) = List.filter (fn x=>Char.isUpper(String.sub(x,0))) xs

(*2*)
fun longest_string1 (xs) = List.foldl
	(fn (x,y)=>if String.size x > String.size y then x else y) "" xs

(*3*)
fun longest_string2 (xs) = List.foldl
	(fn (x,y)=>if String.size x >= String.size y then x else y) "" xs

(*4*)
fun longest_string_helper f xs = 
	List.foldl (fn(x,y)=>if f(String.size x,String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn(x,y)=>x>y)
val longest_string4 = longest_string_helper (fn(x,y)=>x>=y)

(*5*)
val longest_capitalized = longest_string1 o only_capitals

(*6*)
val rev_string = String.implode o List.rev o String.explode

(*7*)
fun first_answer f xs =
	case xs of
		[] => raise NoAnswer |
		x::xs => case f x of
			SOME v => v |
			NONE => first_answer f xs

(*8*)
fun all_answers f xs = 
	let
		fun helper(f, xs, acc) =
			case xs of
				[] => SOME acc |
				x::xs => case f x of
					NONE => NONE |
					SOME lst => helper(f, xs, lst@acc)
	in
		helper(f, xs, [])
	end

(*9a*)
val count_wildcards = g (fn ()=>1) (fn _=>0)

(*9b*)
val count_wild_and_variable_lengths = g (fn ()=>1) (fn str=>String.size(str))

(*9c*)
fun count_some_var (str, pat) = g (fn ()=>0) (fn s=>if s=str then 1 else 0) pat

(*10*)
val check_pat =
	let
		fun has_repeats xs = 
			case xs of
				[] => false |
				head::tail => (List.exists (fn x=>head=x) tail) orelse has_repeats tail

		fun all_strings pat =
			case pat of
			    Wildcard          => []
			  | Variable x        => [x]
			  | TupleP ps         => List.foldl (fn (p,i) => (all_strings p) @ i) [] ps
			  | ConstructorP(_,p) => all_strings p
			  | _                 => []		
	in
		not o has_repeats o all_strings
	end

(*11*)
fun match (value, pat) =
	case (value, pat) of
		(_, Wildcard) => SOME [] |
		(value, Variable s) => SOME [(s,value)] |
		(Unit, UnitP) => SOME [] |
		(Const n1, ConstP n2) => if n1=n2 then SOME [] else NONE |
		(Tuple t1, TupleP t2) => (
			if List.length t1 = List.length t2
			then all_answers match (ListPair.zip(t1,t2))
			else NONE ) |
		(Constructor(s2,v), ConstructorP(s1,p)) => (
			if s1=s2 then match(v,p) else NONE ) |
		_ => NONE

(*12*)
fun first_match value patterns = SOME (first_answer (fn x=>match(value,x)) patterns) 
								 handle NoAnswer => NONE 

(*challenge problem, will do later*)
fun typecheck_patterns patterns = 0