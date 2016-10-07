(* Provides the `Pair` structure *)
CM.autoload "$/utils.cm";

(** from Wikipedia:
 *
 *> Conceptually, a merge sort works as follows:
 *>
 *>   Divide the unsorted list into n sublists, each containing 1 element (a list of 1 element is considered sorted).
 *>   Repeatedly merge sublists to produce new sorted sublists until there is only 1 sublist remaining. This will be the sorted list.
 *
 *)

signature LIST_MERGE_SORT =
sig
    (* Merge sort on polymorphic lists *)
    val sort : ('a * 'a -> bool) -> 'a list -> 'a list
end

structure ListMergeSort : LIST_MERGE_SORT =
struct

    (* Divides a list by separating it into even and odd elements.
     *
     * This makes sense for linked lists, since splitting a list by getting half
     * the length requires traversing the entire list to count the length, then
     * traversing half the list to decouple the first half, whereas in this
     * implementation, we split the list in half in a single pass.
     *)
    fun divide ls =
      let fun cons x xs = x :: xs
      in case ls
          of ([] | [_])   => (ls, [])
           | (x::y::rest) => Pair.bimap (cons x, cons y) (divide rest)
      end

    fun sort compare ls =
      let
          fun merge lists =
            case lists
             of ([], ys)       => ys
              | (xs, [])       => xs
              | (x::xs, y::ys) =>
                if  compare (x, y)
                then y :: (merge (x::xs, ys))
                else x :: (merge (xs, y::ys))
      in
          if length ls < 2
          then ls
          else (merge o Pair.map (sort compare) o divide) ls
      end

end (* ListMergeSort *)

