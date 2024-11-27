let rec lang1 = function
  | ['0'] | ['1'] -> true
  | [] -> false
  | a::l -> 
      if a = '0' || a = '1' 
        then lang1 l 
      else false;;

let lang2 = function
  | [] -> true
  | _::l -> 
      let rec aux = function
        | [] -> true
        | '1'::l' -> aux l'
        | _ -> false
      in aux l;; 

let lang3 = function
  | '0'::l -> 
      let rec aux = function
        | ['0'] -> true
        | b::l' -> 
            if b = '0' || b = '1' then
              aux l'
            else false
        | _ -> false
      in aux l
  | _ -> false;;
  
let lang4 l = List.fold_left(fun x y -> if y = '1' && x >= 0 then x+1 else if y = '0' then x else -1) 0 l = 2;;

let rec lang5 = function
  | a::b::l when (a = '0' || a = '1') && a = b -> 
      if l = [] then true
      else lang5 l
  | _ -> false;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers