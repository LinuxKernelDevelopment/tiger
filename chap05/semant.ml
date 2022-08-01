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
