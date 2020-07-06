(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*a*)
(*string * string list -> string list option*)
fun all_except_option (name, name_list) =
	case name_list of
		[] => NONE |
		x::xs => 
			if same_string(name,x)
			then SOME(xs)
			else
				let val rem = all_except_option(name,xs)
				in case rem of
					NONE => NONE |
					SOME(some) => SOME(x::some)
				end

(*b*)
(*string list list * string -> string list*)
fun get_substitutions1(substitutions, s) =
	case substitutions of
		[] => [] |
		x::xs => case all_except_option(s,x) of
			NONE => get_substitutions1(xs, s) |
			SOME(ans) => ans @ get_substitutions1(xs, s)

(*c*)
(*string list list * string -> string list*)
fun get_substitutions2(substitutions, s) =
	let fun aux(xs,acc) =
		case xs of
			[] => acc |
			x::xs' => case all_except_option(s,x) of
				NONE => aux(xs',acc) |
				SOME(ans) => aux(xs', acc @ ans)
	in
		aux(substitutions, [])
	end

(*d*)
(*string list list * {first:string, last:'a, middle:'b} -> {first:string, last:'a, middle:'b} list*)
fun similar_names(substitutions, {first=a,middle=b,last=c}) =
	let
		fun strListToFullNameList(strList) = 
			case strList of 
				[] => [] |
				x::xs => {first=x,middle=b,last=c}::strListToFullNameList(xs)
	in
		strListToFullNameList(a::get_substitutions2(substitutions,a))
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*a*)
fun card_color (card) =
	case card of
		(Clubs, _) => Black |
		(Spades, _) => Black |
		_ => Red

(*b*)
fun card_value (card) =
	case card of
		(_,Num n) => n |
		(_,Ace) => 11 |
		_ => 10

(*c*)
fun remove_card (cs, c, e) =
	case cs of
		[] => raise e |
		x::xs => if x=c then xs else x::remove_card(xs,c,e)

(*d*)
fun all_same_color (cards) =
	case cards of
		[] => true |
		x::[] => true |
		x::y::xs => if card_color(x)=card_color(y) andalso all_same_color(y::xs)
					then true
					else false

(*e*)
fun sum_cards (cards) =
	let fun aux(cards, acc) =
		case cards of
			[] => acc |
			x::xs => aux(xs, acc + card_value(x))
	in aux(cards, 0) end

(*f*)
fun score (held_cards, goal) =
	let
		val sum = sum_cards(held_cards)
		val prelim = if sum > goal then 3 * (sum - goal) else (goal - sum)
	in
		if all_same_color(held_cards) then prelim div 2 else prelim
	end

(*g*)
fun officiate (card_list, move_list, goal) =
	let
		fun helper(card_list, move_list, held_cards) =
			case move_list of
				[] => score(held_cards, goal) |
				(Discard card) ::xs => 
					helper(card_list, xs, remove_card(card_list, card, IllegalMove)) |
				Draw::xs => case card_list of
					[] => score(held_cards, goal) |
					x::xs' => if sum_cards(x::held_cards) > goal
							  then score(x::held_cards, goal)
							  else helper(xs', xs, x::held_cards)
	in
		helper(card_list, move_list, [])
	end

(*challenge problems will complete later*)
fun score_challenge() = 0
fun officiate_challenge() = 0
fun careful_player() = 0