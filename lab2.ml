(*

Premise 1    Premise 2
-----------------------
Conclusion


Gamma |- m : A       Gamma, x : A |- n : B
-------------------------------------------
Gamma |- let x = m in n : B

. |- let x = 1 in (x + 1) : int

-->

                            ., x : int |- x : int          ., x : int |- 1 : int (helps conclude type of (x + 1))
                          -------------------------------------------------------
. |- 1 : int  (axiom)        ., x : int |- (x + 1) : int
--------------------------------------------------
. |- let x = 1 in (x + 1) : int



                                        x : int, y : int |- x : int (why x?)
                                       -----------------------------------
                 x : int |- 2 : int     x : int, y : int |- (x + y) : int
                ----------------------------------------------------------
. |- 1 : int     x : int |- let y = 2 in (x + y) : int
-------------------------------------------------------
. |- let x = 1 in let y = 2 in (x + y) : int


eval rules

m $ v1             [v1/x]n $ v2
---------------------------------------------
let x = m in n $(represents the down arrow) v2

addition:

m $ v1       n $ v2
---------------------
m + n $ (v1 \+ v2)


            1 $ 1      1 $ 1
            ---------------------------
1 $ 1       (1 + 1) $ 2
--------------------------
let x = 1 in (x + 1) $ 2



            1 $ 1   1 $ 1
            --------------
1 $ 1       1 + 1 $ 2
---------------------
let z = 1 in z + 1 $ 2          2 $ 2
----------------------------------------------------
let x = let z = 1 in z + 1 in z $ 2


*)


type myList =
  | Nil (*capital letters = constructors*)
  | Cons of int * myList

let xs = Nil
let ys = Cons (1, xs)
let zs = Cons (2, ys)

(*can keep creating these constructors to form a list*)
let ls = Cons(1, Cons (2, Cons (3, Nil)))

(*in OCaml, the way the list type is defined:*)
type 'a list = 
  | []
  | (::) of 'a * 'a list

let ls = 1 :: (2 :: (3 :: []))

let ls = [1; 2; 3] (*use semicolons, commas make it interpreted as a tuple*)

type tree = 
  | Leaf
  | Node of tree * int * tree

let tr1 = Leaf

let tr2 = Node (tr1, 3, tr1)

let tr3 = Node (tr2, 2, tr1)


type animal = 
  | Dog of string
  | Cat of string * int
  | Cow of string * float

let animal1 = Dog "Doug"
let animal2 = Cat ("Sally", 23)
let animal3 = Cow ("Bill", 500.)

let get_name (x : animal ) : string = 
  match x with
  | Dog name -> name
  | Cat (name, temp) -> name
  | Cow (name, weight) -> name