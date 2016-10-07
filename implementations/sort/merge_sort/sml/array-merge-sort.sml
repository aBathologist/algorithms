(* Provides the ArraySlicePlus and Pair structures *)
CM.autoload "$/utils.cm";

signature ARRAY_MERGE_SORT =
sig
    val sort : int Array.array -> int Array.array
end

structure ArrayMergeSort : ARRAY_MERGE_SORT =
struct

(* This is a go at an imperative implementation using mutable integer arrays.
 * My principle goal was to implement a merge algorithm that strongly resembled
 * the given pseudocode. To this end, I made a tiny, ugly, weird DSL. *)

structure A = Array
structure AS = ArraySlicePlus

(* Silly imperative syntax sugar  *)
fun ++ n = ( n := (!n) + 1
           ; () )

infix 2 ::=
fun (arr, index) ::= x =
  ( A.update (arr, !index, x)
  ; () )

val return = Fn.id

infix 1 from_upTo_do
fun (n, limit) from_upTo_do f =
  let
      fun loop () =
        if !n >= limit
        then ()
        else ( f ()
             ; ++ n
             ; loop () )
  in
      loop ()
  end

(* The actual implementation *)
fun divide arr =
  let
      val half  = (A.length arr) div 2
      val front = (AS.array o AS.slice) (arr, 0, SOME half)
      val back  = (AS.array o AS.slice) (arr, half, NONE)
  in
      (front, back)
  end

(* Pseudocode for merge
       from https://www.coursera.org/learn/algorithm-design-analysis/

        C = output [length = n]
        A = 1st sorted array [n/2]
        B = 2nd sorted array [n/2]
        i = 1
        j = 1

        ----

        for k = 1 to n
          if A(i) < B(j)
            C(k) = A(i)
            i++
          else [B(j) < A(i)]
            C(k) = B(j)
            j++
        end
 *)

fun merge (arrA, arrB) =
  let

      val n = A.length arrA + A.length arrB
      val C = A.array (n, 0)
      val i = ref 0
      val j = ref 0
      val k = ref 0

      (* Silly syntax sugar *)
      fun A index = A.sub (arrA, !index)
      fun B index = A.sub (arrB, !index)

  in
      (* Translation from the pseudocode (with edge cases) *)
      (k, n) from_upTo_do
             ( fn _ =>
                  if      !j >= A.length arrB then ( (C,k) ::= A(i) ; ++ i)
                  else if !i >= A.length arrA then ( (C,k) ::= B(j) ; ++ j)
                  else if
                      A(i) < B(j)
                  then
                      ( (C,k) ::= A(i)
                      ; ++ i )
                  else
                      ( (C,k) ::= B(j)
                      ; ++ j )
             );
      return C
  end

fun sort arr =
  if A.length arr < 2
  then arr
  else (merge o Pair.map sort o divide) arr

end (* ArrayMergeSortSlice *)
