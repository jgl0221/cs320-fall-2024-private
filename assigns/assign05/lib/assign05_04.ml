(* Definition of the set_info record type *)
type set_info = {
  ind : int -> bool;  (* Indicator function for set membership *)
  mn : int;           (* Minimum value in the set *)
  mx : int;           (* Maximum value in the set *)
}

(* Implementation of ListSet module *)
module ListSet = struct
  type t = int list  (* Represents a set as a list of integers *)

  (* Dummy implementations of required functions *)
  let empty : t = []  (* Dummy empty set *)

  let add x lst = x :: lst  (* Dummy: adds x to the list *)
  
  let remove x lst = List.filter ((<>) x) lst  (* Dummy: removes x from the list if present *)
  
  let mem x lst = List.mem x lst  (* Dummy: checks if x is in the list *)

  let inter lst1 lst2 = List.filter (fun x -> List.mem x lst2) lst1  (* Dummy: returns intersection *)

  let diff lst1 lst2 = List.filter (fun x -> not (List.mem x lst2)) lst1  (* Dummy: returns difference *)

  let singleton x = [x]  (* Dummy: returns a singleton list *)

  let card lst = List.length lst  (* Dummy: returns the length of the list *)

  let union lst1 lst2 = lst1 @ lst2  (* Dummy: concatenates lists *)

  (* Dummy uses of functions to avoid unused warnings *)
  let () =
    let _ = add 1 empty in  (* Example usage of add *)
    let _ = remove 1 empty in  (* Example usage of remove *)
    let _ = mem 1 empty in  (* Example usage of mem *)
    let lst1 = [1; 2; 3] in
    let lst2 = [2; 3; 4] in
    let _ = inter lst1 lst2 in  (* Example usage of inter *)
    let _ = diff lst1 lst2 in  (* Example usage of diff *)
    let _ = card lst1 in  (* Example usage of card *)
    let _ = union lst1 lst2 in  (* Example usage of union *)
    let _ = singleton 5 in  (* Example usage of singleton *)
    ()
end

(* Implementation of FuncSet module *)
module FuncSet = struct
  type t = set_info  (* Represents a set using the set_info record *)

  (* Dummy implementations of required functions *)
  let empty : t = { ind = (fun _ -> false); mn = 0; mx = -1 }  (* Dummy empty set *)

  let add s = s  (* Dummy: does nothing *)
  
  let remove s = s  (* Dummy: does nothing *)

  let mem x s = s.ind x  (* Dummy: checks if x is in the set *)

  let inter = empty  (* Dummy: returns empty set *)

  let diff = empty  (* Dummy: returns empty set *)

  let singleton x = { ind = (fun i -> i = x); mn = x; mx = x }  (* Dummy singleton implementation *)

  let card s = if s.mn <= s.mx then 1 else 0  (* Dummy: returns 1 if not empty, otherwise 0 *)

  let union s1 s2 = 
    { ind = (fun i -> s1.ind i || s2.ind i); mn = min s1.mn s2.mn; mx = max s1.mx s2.mx }  (* Dummy: combines sets *)

  (* Dummy uses of functions to avoid unused warnings *)
  let () =
    let _ = add empty in  (* Example usage of add *)
    let _ = mem 1 empty in  (* Example usage of mem *)
    let _ = remove empty in  (* Example usage of remove *)
    let _ = inter in  (* Example usage of inter *)
    let _ = diff in  (* Example usage of diff *)
    let _ = card empty in  (* Example usage of card *)
    let _ = union empty empty in  (* Example usage of union *)
    let _ = singleton 5 in  (* Example usage of singleton *)
    ()
end
