(* Given n empty glasses with capacities [x1, ... , xn] and operations :
   1. Fill glass i (glass i now has xi water in it)
   2. Empty glass i (glass i now has 0 water in it)
   3. Pour water from glass i to glass j until
      glass i is empty or glass j is full 
   Check if by doing those operations we can achieve capacities [y1, ..., yn] 
   and return minimum number of operations required or -1 if not possible *)

let rec nwd a b = 
	if b = 0 then a
	else nwd b (a mod b)


(* If no glass is empty or full at the end - not possible
   If c = GCD(x1, ..., xn) <> GCD(y1, ..., yn, c) - not possible *)
   
let przelewanka tab = 
	let n = Array.length tab in  
	let empty_full = Array.fold_left (fun a (x,y) -> a || (x = y || y = 0)) (n = 0) tab in
	let tmp = if n = 0 then 1 else fst tab.(0) in
	let nwd1 = Array.fold_left (fun a (x,y) -> nwd a x) tmp tab in
	let nwd2 = Array.fold_left (fun a (x,y) -> nwd a y) nwd1 tab in 
	let q = Queue.create ()
	and used = Hashtbl.create n
	and odl = ref 0 
	and tab2 = Array.init n (fun i -> snd (tab.(i)))
	and tmp = ref (Array.make n 0) in
	begin
		if (nwd1 <> nwd2 || not empty_full) then -1
		else
		begin
			Queue.push (Array.make n 0) q;
			Hashtbl.add used (Array.make n 0) (!odl);
			while (not (Queue.is_empty q)) do
			if (Hashtbl.mem used tab2) then (Queue.clear q;) (* we have our result *)
			else
			begin
				let stan = Queue.pop q in let dodaj tab odl = begin
				if (not (Hashtbl.mem used stan)) then
					(Hashtbl.add used (Array.copy tab) (odl); 
					Queue.push (Array.copy tab) q;) end; in
				begin
					tmp := Array.copy stan;
					odl := Hashtbl.find used stan;
					for i = 0 to (n - 1) do
						let bef = stan.(i) in 
						begin
							stan.(i) <- fst (tab.(i)); (* fill every glass *)
							dodaj (Array.copy stan) (!odl + 1);
							stan.(i) <- 0; (* empty every glass *)
							dodaj (Array.copy stan) (!odl + 1);
							stan.(i) <- bef;
						end;
						for j = 0 to (n - 1) do
							let maxim = fst tab.(j) and befj = stan.(j) and befi = stan.(i) in
							if maxim > befj && i <> j then
							begin
							(* make every possible move *)
								stan.(j) <- min maxim (befj + befi);
								stan.(i) <- befi - stan.(j) + befj;
								dodaj (Array.copy stan) (!odl + 1);
								stan.(j) <- befj;
								stan.(i) <- befi;
							end;
						done;
					done;
				end;
			end;	
			done;
		(* if our end state is not possible to achieve return -1 *)
		try (Hashtbl.find used tab2) with Not_found -> -1
		end;
	end;;
