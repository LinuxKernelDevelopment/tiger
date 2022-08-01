
type 'a tree = LEAF | TREE of 'a tree * 'a * 'a tree

let empty = LEAF

let rec insert key = function
        | LEAF -> TREE (LEAF, key, LEAF)
        | TREE (l, k, r) -> (
                              if key < k then
                                TREE (insert key l, k, r)
                              else if key > k then
                                TREE (l, k, insert key r)
                              else
                                TREE (l, key, r)
                            )

let rec mem key = function
        | LEAF -> false
        | TREE (l, k, r) -> (
                              if key = fst k then
                                true
                              else if key < fst k then
                                mem key l
                              else
                                mem key r
                            )

let rec lookup tree key = match tree with
        | LEAF -> raise Not_found
        | TREE (l, k, r) -> (
                              if key = fst k then
                                  k
                              else if key < fst k then
                                lookup l key
                              else
                                lookup r key
                            )