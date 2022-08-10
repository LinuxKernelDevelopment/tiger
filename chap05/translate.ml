module T = Tree

type level =
    Outermost
  | Level of level * Frame.frame * unit ref

type access = level * Frame.access

type exp =
    Ex of T.exp
  | Nx of T.stm
  | Cx of (Temp.label -> Temp.label -> T.stm)

type frag = Frame.frag

let frags = ref []

let get_result () = !frags

let outermost = Outermost

let new_level parent name formals =
  (* Include static link in formals *)
  let formals' = true :: formals in
  let frame = Frame.new_frame name formals' in
    Level (parent, frame, ref ())

let formals = function
    Outermost -> []
  | Level (_, frame, _) as level ->
      (* Remove static link from formals *)
      let formals' = (List.tl (Frame.formals frame)) in
        List.map (fun access -> (level, access)) formals'
