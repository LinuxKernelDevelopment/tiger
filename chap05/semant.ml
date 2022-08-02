module A = Absyn
module S = Symbol
module T = Types

type venv = Env.enventry Symbol.table
type tenv = Types.t Symbol.table
type expty = {exp: Translate.exp; ty: Types.t}
type decenv = {venv: venv; tenv: tenv; exps: Translate.exp list}

let checkexpty ty1 {exp; ty=ty2} pos =
  match ty1, ty2 with
  | T.RECORD _ , T.NIL      -> ()
  | T.NIL      , T.RECORD _ -> ()
  | T.RECORD (_, unique1), T.RECORD (_, unique2) ->
      if unique1 != unique2
      then Error_msg.error pos (Error_msg.Record_type_mismatch)
  | T.ARRAY (_, unique1)  , T.ARRAY (_, unique2) ->
      if unique1 != unique2
      then Error_msg.error pos (Error_msg.Array_type_mismatch)
  | _ ->
      if ty1 <> ty2
      then Error_msg.error pos (Error_msg.Type_mismatch (T.string_of_ty ty1, T.string_of_ty ty2))

let checkint = check_expty T.INT

let checkunit = check_expty T.UNIT

let checkcomp lty rty pos =
  match lty, rty with
  | T.RECORD _, T.NIL       -> ()
  | T.NIL     , T.RECORD _  -> ()
  | T.RECORD _, T.RECORD _  -> ()
  | T.ARRAY _ , T.ARRAY _    -> ()
  | T.INT     , T.INT        -> ()
  | T.STRING  , T.STRING     -> ()
  | _ -> Error_msg.error pos
           (Error_msg.Illegal_comparison (T.string_of_ty lty, T.string_of_ty rty))

let rec actual_ty = function
  | T.NAME (_, ty) ->
      begin
        match !ty with
          None -> Error_msg.impossible "name type without actual type"
        | Some ty' -> actual_ty ty'
      end
  | ty' -> ty'

let rec trans_var venv tenv break level = function
  | A.SimpleVar (id, pos) ->
      begin
        match S.look id venv with
          Some (Env.VarEntry (access, ty)) ->
            {exp = Translate.simple_var access level; ty}
        | _ ->
            Error_msg.error pos (Error_msg.Undefined_variable (S.name id));
            {exp = Translate.unit_exp; ty = T.INT}
      end
  | A.FieldVar (var, id, pos) ->
      let {exp = record_exp; ty = record_ty} = trans_var venv tenv break level var in
        begin
          match record_ty with
          | T.RECORD (fields, _) ->
              begin
                try
                  let (i, (_, ty)) = List.find
                      (fun (_, (name, _)) -> name = id)
                      (List.mapi (fun i e -> (i, e)) fields) in
                    {exp = Translate.field_var record_exp i; ty = actual_ty ty}
                with Not_found ->
                  Error_msg.error pos (Error_msg.Undefined_record_field (S.name id));
                  {exp = Translate.unit_exp; ty = T.INT}
              end
          | ty ->
              Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
              {exp = Translate.unit_exp; ty = T.INT}
        end
  | A.SubscriptVar (var, i, pos) ->
     let {exp = array_exp; ty = array_ty} = trans_var venv tenv break level var in
       begin
         match actual_ty array_ty with
         | T.ARRAY (elem_ty, _) ->
             let {exp = subscript_exp; ty = subscript_ty} as subscript_expty = trans_exp venv tenv break level i in
               checkint subscript_expty pos;
               let exp = Translate.subscript_var array_exp subscript_exp in
               let ty = actual_ty elem_ty in
                 {exp; ty}
         | ty ->
             Error_msg.error pos (Error_msg.Type_mismatch ("array", T.string_of_ty ty));
             {exp = Translate.unit_exp; ty = T.INT}
       end

and trans_exp venv tenv break level exp =
  let rec trexp = function
    | A.VarExp var ->
        trans_var venv tenv break level var
    | A.NilExp ->
        {exp = Translate.nil_exp; ty = T.NIL}
    | A.IntExp i ->
        {exp = Translate.int_exp i; ty = T.INT}
    | A.StringExp (str, _) ->
        {exp = Translate.string_exp str; ty = T.STRING}
    | A.CallExp (name, args, pos) ->
        begin
          match S.look name venv with
          | Some (Env.FunEntry (flevel, label, formal_tys, result_ty)) ->
             let numformals = List.length formal_tys in
             let numactuals = List.length args in
               if numformals <> numactuals then
                 begin
                   Error_msg.error pos (Error_msg.Arity_mismatch (numformals, numactuals));
                   {exp = Translate.unit_exp; ty = T.INT}
                 end
               else
                 let arg_exps = List.map2
                     (fun formal_ty arg ->
                        let {exp; _ } as expty = trexp arg in check_expty formal_ty expty pos; exp)                     formal_tys
                     args in
                    {exp = Translate.call_exp flevel level label arg_exps; ty = result_ty}
          | _ ->
              Error_msg.error pos (Error_msg.Undefined_function (S.name name));
              {exp = Translate.unit_exp; ty = T.INT}
         end
    | A.OpExp (l, op, r, pos) when op = A.EqOp || op = A.NeqOp ->
        let {exp = lexp; ty = lty} = trexp l in
        let {exp = rexp; ty = rty} = trexp r in
          checkcomp lty rty pos;
          let exp = match op, lty, rty with
              A.EqOp, T.STRING, T.STRING -> Translate.string_eq_op_exp lexp rexp
            | A.NeqOp, T.STRING, T.STRING -> Translate.string_ne_op_exp lexp rexp
            | A.Eqop, _, _ -> Translate.eq_op_exp lexp rexp
            | A.NeqOp, _, _ -> Translate.ne_op_exp lexp rexp
            | _ -> raise (Failure "trans_exp")
          in {exp; ty = T.INT}
    | A.OpExp (l, op, r, pos) ->
        let {exp = lexp; _} as lexpty = trexp l in
        let {exp = rexp; _} as rexpty = trexp r in
        checkint lexpty pos;
        checkint rexpty pos;
        let exp = match op with
            A.PlusOp -> Translate.plus_op_exp lexp rexp
          | A.MinusOp -> Translate.minus_op_exp lexp rexp
          | A.TimesOp -> Translate.mul_op_exp lexp rexp
          | A.DivideOp -> Translate.div_op_exp lexp rexp
          | A.LtOp -> Translate.lt_op_exp lexp rexp
          | A.LeOp -> Translate.le_op_exp lexp rexp
          | A.GtOp -> Translate.gt_op_exp lexp rexp
          | A.GeOp -> Translate.ge_op_exp lexp rexp
          | _ -> raise (Failure "trans_exp")
        in {exp; ty = T.INT}
    | A.RecordExp (fields, ty_id, pos) ->
        begin
          match S.look ty_id tenv with
          | Some record_ty ->
              begin
                match actual_ty record_ty with
                | T.RECORD (field_tys, _) as record_ty' ->
                    let field_exps = List.map2
                        (fun (name, ty) (name', exp, pos) ->
                           if name <> name' then
                             Error_msg.error pos (Error_msg.Record_field_mismatch (S.name name, S.name name'));
                           let {exp = field_exp; ty = field_ty} as field_expty = trexp exp in
                             check_expty (actual_ty ty) field_expty pos;
                             field_exp)
                        field_tys
                        fields
                    in {exp = Translate.record_exp field_exps; ty = record_ty'}
                | ty ->
                    Error_msg.error pos (Error_msg.Type_mismatch ("record", T.string_of_ty ty));
                    {exp = Translate.unit_exp; ty = T.RECORD ([], ref ())}
              end
          | None ->
              Error_msg.error pos (Error_msg.Undefined_record (S.name ty_id));
              {exp = Translate.unit_exp; ty = T.RECORD([], ref ())}
        end
    | A.SeqExp (exps) ->
        let (exps, ty) = List.fold_left
            (fun (exps, _) (e, _) -> let {exp; ty} = trexp e in (exps @ [exp], ty))
            ([], T.UNIT)
            exps in
          {exp = Translate.seq_exp exps; ty}
    | A.AssignExp (var, exp, legal, pos) ->
        if not !legal then Error_msg.error pos Error_msg.Illegal_assignment;
        let {exp = var_exp; ty} = trans_var venv tenv break level var in
        let {exp = val_exp; _} as val_expty = trexp exp in
          check_expty ty val_expty pos;
          {exp = Translate.assign_exp var_exp val_exp; ty = T.UNIT}
    | A.IfExp (test, conseq, Some alt, pos) ->
        let {exp = test_exp; _} as test_expty = trexp test in
        let {exp = conseq_exp; ty = conseq_ty} = trexp conseq in
        let {exp = alt_exp; _} as alt_expty = trexp alt in
          checkint test_expty pos;
          check_expty conseq_ty alt_expty pos;
          {exp = Translate.if_exp3 test_exp conseq_exp alt_exp; ty = conseq_ty}
    | A.ForExp (var, escape, lo, hi, body, pos) ->
        let limit_sym = Symbol.symbol "*limit*" in
        let lo_sym = Symbol.symbol "*lo*" in
        let hi_sym = Symbol.symbol "*hi*" in
        let decs = [A.VarDec {A.vardec_name = lo_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = lo;
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = hi_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = hi;
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = var;
                              vardec_escape = escape;
                              vardec_ty = None;
                              vardec_init = A.VarExp (A.SimpleVar (lo_sym, pos));
                              vardec_pos = pos};
                    A.VarDec {A.vardec_name = limit_sym;
                              vardec_escape = ref false;
                              vardec_ty = None;
                              vardec_init = A.VarExp (A.SimpleVar (hi_sym, pos));
                              vardec_pos = pos}] in
        let body' = A.IfExp (
            A.OpExp (A.VarExp (A.SimpleVar (lo_sym, pos)), A.LeOp, A.VarExp (A.SimpleVar (hi_sym, pos)), pos),
            A.WhileExp (
              A.IntExp 1,
              A.SeqExp [
                (A.IfExp (A.IntExp 1, body, None, pos), pos);
                (A.IfExp (
                   A.OpExp (A.VarExp (A.SimpleVar (var, pos)), A.LtOp, A.VarExp (A.SimpleVar (limit_sym, pos)), pos),
                   A.AssignExp (A.SimpleVar (var, pos), A.OpExp (A.VarExp (A.SimpleVar (var, pos)), A.PlusOp, A.IntExp 1, pos), ref true, pos),
                   pos
                   ), pos)
              ],
              pos
            ),
            None,
            pos) in
          trexp (A.LetExp (decs, body', pos))
