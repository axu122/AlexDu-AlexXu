(* file: part1.ml
   author: Alex Xu
   This file contains problems for problem set 4 of CSCI1013 at
   Boston College. It depends on Cs1103.ml (local to Boston College).
   Usage:
   > make
   > ./part1
*)

open World
open Color
open Image
open Cs1103
open Assert

(* removeNth : a' list -> int -> a' list
   elementRemoved :  int -> (a' -> bool) -> a' list -> a' list
   - this function stops and compares nth element from a list
   (denoted by the "int" input) and compares it to itself, creating a bool
   argument (denoted by a' -> bool). When test returns "true", new list is
   formed from the filtration of the nth element
*)
  let removeNth givenlist element =
    let rec elementRemoved xs element =
      match xs , element >= 0 with
      | [] , _ -> failwith "List is empty, no element to remove"
      | _ , false -> failwith "There is no negative element"
      | first :: restoflist , true ->
        (match element = 0 with
        |true -> List.filter (fun x -> (x <> first) ) givenlist
        |false -> elementRemoved restoflist (element - 1))
        in
        elementRemoved givenlist element


(* numOfFactor: int -> int
   - Takes in an int, and find # factors of said int
   howManyFactors : int list -> tupple list
   - Takes in a integer list, seperates each element into a tupple, where
   second element in tupple is the # of factors. *)



let rec numOfFactor n =
  let m = n in
  let rec repeat n =
  match n = 0 with
  |true -> 0
  |false -> match ((m mod (m - n + 1)) = 0) with
           |true -> 1 + repeat(n-1)
           |false -> repeat(n-1)
in repeat n



let rec howManyFactors xs =
  match xs with
  |[] -> []
  |x::xs -> (x, numOfFactor x) :: howManyFactors xs




(* Test removeNth
*)
type parts = A (* removeNthTest *)
           | B (* howManyFactors *)

let removeNthTest1 () = (removeNth [1; 2; 3] 2) = [1; 2]
let removeNthTest2 () =  (removeNth [3; 5; 6; 8; 9] 4) = [3; 5; 6; 8]
let removeNthTest3 () =  (removeNth ['a'; 'b'; 'c'] 1) = ['a'; 'c']
let removeNthTest () =
  Assert.run_test "removeNth test1" removeNthTest1;
  Assert.run_test "removeNth test2" removeNthTest2;
  Assert.run_test "removeNth test3" removeNthTest3


(* Test howManyFactors
*)

let list1 = [4; 5; 6]
let list2 = [10; 11; 12; 13; 14; 15]
let list3 = [32; 87; 93; 101; 315]
let howManyFactorsTest1 () = (howManyFactors list1) = [(4, 3); (5, 2); (6, 4)]
let howManyFactorsTest2 () =  (howManyFactors list2) = [(10, 4); (11, 2); (12, 6); (13, 2); (14, 4); (15, 4)]
let howManyFactorsTest3 () =  (howManyFactors list3) = [(32, 6); (87, 4); (93, 4); (101, 2); (315, 12)]
let howManyFactorsTest () =
  Assert.run_test "howManyFactors list1" howManyFactorsTest1;
  Assert.run_test "howManyFactors list2" howManyFactorsTest2;
  Assert.run_test "howManyFactors list3" howManyFactorsTest3


let test part =
  match part with
  | A -> removeNthTest ()
  | B -> howManyFactorsTest ()

let () = test A
let () = test B
