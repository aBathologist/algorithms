(* Provides ArraySlicePlus structure *)
CM.autoload "$/utils.cm";

structure ListQuickSort =
struct
    (* Naive, non-tail recursive implementation, using O(n) memory *)
    fun sort compare ls =
      let fun sort [] = []
            | sort (x::xs) =
              let val (greater, less) = List.partition (Fn.curry compare x) xs
              in (sort less) @ [x] @ (sort greater)
              end
      in sort ls
      end
end

structure ArrayQuickSort =
struct
    (* In-place sort using mutable arrays *)
    structure A = ArraySlicePlus
    val sub = A.subOpt

    (* Uses array-slices to absract away some indexing *)
    fun partition arr =
      case A.getItem arr
       of NONE => NONE
        | SOME (pivot, partitioned) =>
          let fun partition' (i,j) =
                case sub partitioned j
                 of NONE     => A.splitAround i (A.swap arr (0, i))
                  | SOME jth =>
                    if jth < pivot
                    then ( A.swap partitioned (i,j)
                         ; partition' (i+1, j+1) )
                    else partition' (i, j+1)
          in
              partition' (0,0)
          end

    fun sort arr =
      let fun sort' arr =
            case partition arr
             of NONE => ()
              | SOME (left, right) => (sort' left; sort' right)
      in ( sort' (A.full arr)
         ; arr )
      end

end (* ArrayQuickSort *)
