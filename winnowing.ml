open Core

let rec remove_adjacent_duplicates nums = match nums with
	| [] -> []
	| x::[] -> [x]
	| x::xx::xs -> if x = xx then remove_adjacent_duplicates (x::xs) else x::(remove_adjacent_duplicates (xx::xs))

let winnow nums ~k =
	let deque = Deque.create () in
	let result_indices_mut = ref [] in
	for i = 0 to (List.length nums) - 1 do
		(if i >= k && (Deque.peek_front_exn deque) <= (i - k) then
			Deque.drop_front ~n:1 deque;
		);
		(while not (Deque.is_empty deque) && (List.nth_exn nums (Deque.peek_back_exn deque)) >= (List.nth_exn nums i) do
			Deque.drop_back ~n:1 deque;
		done);
		Deque.enqueue_back deque i;
		(if i >= k - 1 then
			result_indices_mut := (!result_indices_mut @ [Deque.peek_front_exn deque]);
		);
	done;
	let result_indices = !result_indices_mut in
	let result_indices_without_adj_dups = remove_adjacent_duplicates result_indices in
	let result = List.map ~f:(fun i -> List.nth_exn nums i) result_indices_without_adj_dups in
	result

