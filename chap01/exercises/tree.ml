
type key = string
type tree = LEAF | TREE of tree * key * tree

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
                              if key = k then
                                true
                              else if key < k then
                                mem key l
                              else
                                mem key r
                            )