open Core

let rec remove_adjacent_duplicates nums = match nums with
	| [] -> []
	| x::[] -> [x]
	| x::xx::xs -> if x = xx then remove_adjacent_duplicates (x::xs) else x::(remove_adjacent_duplicates (xx::xs))

(* cmp function takes in two 'a and returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second. 
e.g. if a = 1 and b = 2 then cmp a b < 0
*)
let winnow (nums:'a sexp_list) ~(w:int) ~(cmp: 'a -> 'a -> int) =
	let deque = Deque.create () in
	let result_indices_mut = ref [] in
	for i = 0 to (List.length nums) - 1 do
		(if i >= w && (Deque.peek_front_exn deque) <= (i - w) then
			Deque.drop_front ~n:1 deque;
		);
		(while not (Deque.is_empty deque) && (cmp (List.nth_exn nums (Deque.peek_back_exn deque)) (List.nth_exn nums i)) >= 0 do
			Deque.drop_back ~n:1 deque;
		done);
		Deque.enqueue_back deque i;
		(if i >= w - 1 then
			result_indices_mut := (!result_indices_mut @ [Deque.peek_front_exn deque]);
		);
	done;
	let result_indices = !result_indices_mut in
	let result_indices_without_adj_dups = remove_adjacent_duplicates result_indices in
	let result = List.map ~f:(List.nth_exn nums) result_indices_without_adj_dups in
	result

