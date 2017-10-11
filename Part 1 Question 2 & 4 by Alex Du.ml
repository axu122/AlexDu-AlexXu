(* file: part1.ml
   author: Anran Du
   This file contains material for problem set 4 of CSCI1103
   Computer Science I Honors, taught at Boston College.

   Usage:
   > make
   > ./go
*)

open Cs1103
open Assert

(* Problem 2
*)
(* explode : string -> char list
*)
let explode s =
  let rec repeat i s answer =
    match i < 0 with
    | true  -> answer
    | false -> repeat (i - 1) s (s.[i] :: answer)
  in
  repeat (String.length s - 1) s []

(* hammingDistance : string -> string -> int_of_char
   charDifference : char list -> char list -> int
   The charDifference function takes in two char lists, charList1 and charList2.
   charList1 and charList2 are char lists made up of characters from the given two
   strings s1 and s2. By using the recursive function, charDifference keeps matching
   each char from the char lists to see if they have the same value. If they don't,
   1 is added to the result; if they do, 0 is added to the result. Finally, the
   output will be the hamming distance between the two strings.
*)
let hammingDistance s1 s2 =
  let charList1 = explode s1 in
  let charList2 = explode s2 in
  let rec charDifference charList1 charList2 =
              match (charList1, charList2) with
              | ([], []) -> 0
              | ([], _) -> failwith "The two strings do not have the same length."
              | (_, []) -> failwith "The two strings do not have the same length."
              | (x :: xs, y :: ys) -> match x = y with
                                      | true  -> charDifference xs ys
                                      | false -> 1 + charDifference xs ys
  in
  charDifference charList1 charList2

(*Problem 4
*)
(* isPerfect : int -> bool
   buildFactorList : int -> int list
   buildFactorList first find the remainder of n divided by m using the mod function.
   The function then checks if the remainder is equal to 0 and put all the m that
   satisfies into a list. By doing this recursively, the function returns a list
   of all the factors of n.
   The rest of the function isPerfect adds up the ints in buildFactorList and compares
   the result to n. If the result equals to n, the function returns true; if the
   result is not equal to n, the function returns false. 
*)
let isPerfect (n : int) =
  let m = n - 1 in
  let rec buildFactorList m fList =
    let remainder = n mod m
    in
    match (remainder = 0, m = 1) with
    | (true, false) -> buildFactorList (m - 1) (m :: fList)
    | (false, _) -> buildFactorList (m - 1) fList
    | (_, true) -> fList
  in
  let factorList = buildFactorList m [1] in
  let sum = List.fold_left (+) 0 factorList
  in
  sum = n

(* Testing *)

type parts = A (* hammingDistance *)
           | B (* isPerfect *)

(* Test hammingDistance
*)
let hammingDistanceTest1 () = hammingDistance "ABCD" "ABDE" = 2
let hammingDistanceTest2 () = hammingDistance "BCVFG" "ABCDE" = 5
let hammingDistanceTest3 () = hammingDistance "QWERTY" "QWERTG" = 1
let hammingDistanceTest () =
  Assert.run_test "hammingDistance test 1" hammingDistanceTest1;
  Assert.run_test "hammingDistance test 2" hammingDistanceTest2;
  Assert.run_test "hammingDistance test 3" hammingDistanceTest3

(* Test isPerfect
*)
let isPerfectTest1 () = isPerfect 6 = true
let isPerfectTest2 () = isPerfect 28 = true
let isPerfectTest3 () = isPerfect 8 = false
let isPerfectTest () =
  Assert.run_test "isPerfect test 1" isPerfectTest1;
  Assert.run_test "isPerfect test 2" isPerfectTest2;
  Assert.run_test "isPerfect test 3" isPerfectTest3

let test part =
  match part with
  | A -> hammingDistanceTest ()
  | B -> isPerfectTest ()

(* Run the various tests.
*)
let () = test A
let () = test B
