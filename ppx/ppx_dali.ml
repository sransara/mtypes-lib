open Migrate_parsetree
open Ast_402

module Ast_convenience = Ast_convenience_402
open Asttypes
open Parsetree
open Ast_mapper

let raise_errorf ?sub ?if_highlight ?loc message =
  message |> Printf.kprintf (fun str ->
      let err = Location.error ?sub ?if_highlight ?loc str in
      raise (Location.Error err))

let mkaststr str = {txt=str; loc = !Ast_helper.default_loc}

(* To map:
   [%%dali_imodstr_rename]
   [%%dali_mmodsig_functs]
   [%%dali_mmodstr_functs]
   ...
   [%dali_adt_mod] = module
   [%dali_adt_typesig] = 'a t --> A.t t
   [%dali_madt_typedef] = type 'a t = T0 | T1 of 'a * 'a t --> type internal = T0 | T1 of K.t * A.t
   [%dali_aovalue_equal] = match t1, t2 with
   [%dali_aovalue_to_json] = to_json
   [%dali_aovalue_of_json] = of_json
   [%dali_of_adt]
   [%dali_to_adt]
*)

let mk_dali_mapper (mn, mts, mtd, jc, ofa, toa, imr, msigf, mstrf, mergf, mmodi, bcstoi) = {
  default_mapper with 
  module_expr = (fun mapper t ->
      match t with
      | { pmod_desc = Pmod_extension ({txt = "dali_adt_mod"}, _) } -> mn
      | { pmod_desc = Pmod_extension ({txt = "dali_mmod_inst"}, _) } -> mmodi
      | { pmod_desc = Pmod_extension ({txt = "dali_bcsto_inst"}, _) } -> bcstoi
      | x -> default_mapper.module_expr mapper x);
  typ = (fun mapper t ->
      match t with
      | { ptyp_desc = Ptyp_extension ({txt = "dali_adt_typesig"}, _) } -> mts
      | x -> default_mapper.typ mapper x);
  structure_item = (fun mapper t ->
      match t with
      | { pstr_desc = Pstr_extension (({txt = "dali_madt_typedef"}, _), _) } -> mtd
      | { pstr_desc = Pstr_extension (({txt = "dali_json_convert"}, _), _) } -> jc
      | { pstr_desc = Pstr_extension (({txt = "dali_imodstr_rename"}, p), _) } ->
        mapper.structure_item mapper (imr p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mmodsig_functs"}, p), _) } ->
        mapper.structure_item mapper (msigf p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mmodstr_functs"}, p), _) } ->
        mapper.structure_item mapper (mstrf p) 
      | { pstr_desc = Pstr_extension (({txt = "dali_mergeable_functs"}, p), _) } ->
        mapper.structure_item mapper (mergf p) 
      | x -> default_mapper.structure_item mapper x);
  expr = (fun mapper e ->
      match e with
      | { pexp_desc = Pexp_extension ({txt = "dali_of_adt"}, _)} -> ofa
      | { pexp_desc = Pexp_extension ({txt = "dali_to_adt"}, _)} -> toa
      | x -> default_mapper.expr mapper x);
}

let dali_imodstr_rename tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ {x with pmb_name = mkaststr @@ "I" ^ mn }
  | _ -> assert false

let dali_mmodsig_functs tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_modtype x}] ->
    Ast_helper.Str.modtype 
      { x with 
        pmtd_name = (mkaststr @@ "M" ^ String.uppercase mn); 
        pmtd_type = (match x.pmtd_type with
            | Some typ ->
              let rec aux params =
                match params with
                | [] -> typ
                | ({ ptyp_desc = desc },_) :: r ->
                  (match desc with
                   | Ptyp_var s ->
                     let fcname = mkaststr @@ String.uppercase s in
                     let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "Tc.S0" in
                     Ast_helper.Mty.functor_ fcname (Some fcsig) (aux r)
                   | _ -> assert false) in 
              Some (aux td.ptype_params)
            | None -> assert false);
      }
  | _ -> assert false

let dali_mmodstr_functs tds td mn p =
  let mn = String.concat "." (Longident.flatten mn) in
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ 
      { x with 
        pmb_name = (mkaststr @@ "M" ^ mn); 
        pmb_expr = 
          let rec aux params =
            match params with
            | [] -> x.pmb_expr
            | ({ ptyp_desc = desc },_) :: r ->
              (match desc with
               | Ptyp_var s ->
                 let fcname = mkaststr @@ String.uppercase s in
                 let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "Tc.S0" in
                 Ast_helper.Mod.functor_ fcname (Some fcsig) (aux r)
               | _ -> assert false) in 
          let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid ("M" ^ String.uppercase mn) in
          Ast_helper.Mod.constraint_ (aux td.ptype_params) fcsig
      }
  | _ -> assert false

let dali_mergeable_functs tds td mn p =
  match p with
  | PStr [ {pstr_desc = Pstr_module x}] ->
    Ast_helper.Str.module_ 
      { x with 
        pmb_expr = 
          let rec aux params =
            match params with
            | [] -> x.pmb_expr
            | ({ ptyp_desc = desc },_) :: r ->
              (match desc with
               | Ptyp_var s ->
                 let fcname = mkaststr @@ String.uppercase s in
                 let fcsig = Ast_helper.Mty.ident @@ Ast_convenience.lid "MERGEABLE" in
                 Ast_helper.Mod.functor_ fcname (Some fcsig) (aux r)
               | _ -> assert false) in 
          aux td.ptype_params
      }
  | _ -> assert false

let dali_mmod_inst tds td mn =
  let mn = String.concat "." (Longident.flatten mn) in
  let rec aux params =
    match params with
    | [] -> Ast_helper.Mod.ident @@ Ast_convenience.lid ("M" ^ mn) 
    | ({ ptyp_desc = desc },_) :: r ->
      (match desc with
       | Ptyp_var s ->
         let fcid = Ast_helper.Mod.ident @@ Ast_convenience.lid (String.uppercase s) in
         Ast_helper.Mod.apply (aux r) fcid
       | _ -> assert false) in 
  aux td.ptype_params

let dali_bcsto_inst tds td mn =
  let rec aux params =
    match params with
    | [] -> Ast_helper.Mod.ident @@ Ast_convenience.lid ("BC_store") 
    | ({ ptyp_desc = desc },_) :: r ->
      (match desc with
       | Ptyp_var s ->
         let fcid = Ast_helper.Mod.ident @@ Ast_convenience.lid (String.uppercase s) in
         Ast_helper.Mod.apply (aux r) fcid
       | _ -> assert false) in 
  aux td.ptype_params

let dali_adt_mod mn =
  Ast_helper.Mod.ident (Location.mkloc mn !Ast_helper.default_loc)

let dali_adt_typesig tds td mn  =
  let open Ast_helper in
  let type_mapper { ptyp_desc = desc } =
    let open Typ in
    match desc with
    | Ptyp_var s -> constr (Ast_convenience.lid (String.uppercase s ^ ".t")) [] 
    | _ -> assert false in
  let map_fst f (x, y) = f x in
  let original_type_params = List.map (map_fst type_mapper) td.ptype_params  in
  let original_type_ident = Longident.Ldot (mn, td.ptype_name.txt) in
  Typ.constr (Location.mkloc original_type_ident !default_loc) original_type_params

let dali_madt_typedef tds td mn =
  let open Ast_helper in
  let open Ast_helper.Typ in
  let main_td_mapper = {   
    default_mapper with
    typ = (fun mapper t ->
        match t with
        | { ptyp_desc = Ptyp_var x } -> 
          default_mapper.typ mapper @@ constr (Ast_convenience.lid (String.uppercase x ^ ".t")) []
        | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident n }, _) } ->
          default_mapper.typ mapper @@ 
          if n = td.ptype_name.txt then constr (Ast_convenience.lid ("K.t")) []
          else t
        | x -> default_mapper.typ mapper x);
    constructor_declaration = (fun mapper t ->
        let mapf arg =
          match arg with
          | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident n }, _) } ->
            if n = td.ptype_name.txt then constr (Ast_convenience.lid ("K.t")) []
            else arg
          | _ -> arg in
        let cd = { t with pcd_args = List.map mapf t.pcd_args } in
        default_mapper.constructor_declaration mapper cd
      );
  } in 
  let other_td_mapper = {
    default_mapper with
    typ = (fun mapper t ->
        match t with
        | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident n }, _) } ->
          default_mapper.typ mapper @@ 
          if n = td.ptype_name.txt then constr (Ast_convenience.lid ("K.t")) []
          else t
        | x -> default_mapper.typ mapper x);
    constructor_declaration = (fun mapper t ->
        let mapf arg =
          match arg with
          | { ptyp_desc = Ptyp_constr ({ txt = Longident.Lident n }, _) } ->
            if n = td.ptype_name.txt then constr (Ast_convenience.lid ("K.t")) []
            else arg
          | _ -> arg in
        let cd = { t with pcd_args = List.map mapf t.pcd_args } in
        default_mapper.constructor_declaration mapper cd
      );} in
  let type_dec_mapper = 
    {   
      default_mapper with
      type_declaration = (fun mapper t -> 
          match t with
          | { ptype_attributes = [({txt = "derive"}, _) ]; } -> 
            main_td_mapper.type_declaration main_td_mapper 
              { t with ptype_name = mkaststr "madt"; ptype_params = []; ptype_attributes = [] }
          | x -> default_mapper.type_declaration other_td_mapper x);
    } in
  let madts = List.map (type_dec_mapper.type_declaration type_dec_mapper) tds in 
  (Ast_helper.Str.type_ @@ madts, madts) 

let dali_of_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let plhs = pconstr name (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) c.pcd_args) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr c.pcd_name.txt a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "of_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 c.pcd_args [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = fst x in  
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "of_adt") [evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> e.pld_name.txt, e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract ->       
      let matchmaker l =
        let mkarg x = "a" ^ string_of_int x in
        let mkparg x = mkp @@ mkarg x in
        let plhs = ptuple (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; tuple a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "to_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 l [] in
        Exp.match_ (evar ak) [Exp.case plhs prhs] in 
      (match ctd.ptype_manifest with
       | Some ({ptyp_desc = Ptyp_constr (_, _) } as x) -> matchmaker [x]
       | Some {ptyp_desc = Ptyp_tuple l} -> matchmaker l
       | None -> failwith "[dalify] Open abstract types are not supported"
       | _ -> assert false)
    | Ptype_open -> assert false in
  kind_mapper td "t"

let dali_to_adt tds td mn =
  let open Ast_helper in
  let open Ast_convenience in
  let mkp x = x ^ "'" in
  let mn = String.concat "." (Longident.flatten mn) in
  let rec find_some_type l tn =
    match l with
    | [] -> None
    | x :: y ->
      if x.ptype_name.txt = tn then Some x
      else find_some_type y tn in
  let rec kind_mapper ctd ak =
    match ctd.ptype_kind with
    | Ptype_variant l ->
      let mkarg x = "a" ^ string_of_int x in
      let mkparg x = mkp @@ mkarg x in
      let pat_mapper c =
        let name = mn ^ "." ^ c.pcd_name.txt in
        let plhs = pconstr c.pcd_name.txt (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) c.pcd_args) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; constr name a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "to_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 c.pcd_args [] in
        Exp.case plhs prhs in
      Exp.match_ (evar ak)  (List.map pat_mapper l)
    | Ptype_record l ->
      let plhs = precord @@ List.map (fun e -> e.pld_name.txt, pvar e.pld_name.txt) l in
      let prhs = 
        let rec aux args allargns =
          match args with
          | [] ->
            let a = List.map (fun e -> mn ^ "."^ fst e, evar @@ snd e) @@ List.rev allargns in
            app (evar "@@") [evar "Lwt.return"; record a]
          | x :: y ->
            let argn = fst x in  
            match (snd x).ptyp_desc with
            | Ptyp_constr ({txt = Longident.Lident n}, _) ->
              let argnp = mkp argn in
              if n = td.ptype_name.txt then 
                app (evar ">>=") [app (evar "to_adt") [evar argn]; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
              else
                let st = find_some_type tds n in
                (match st with
                 | Some t ->
                   app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux y ((argn,argnp) :: allargns))]
                 | None -> aux y ((argn,argn) :: allargns))
            | _ -> aux y ((argn,argn) :: allargns) in
        aux (List.map (fun e -> e.pld_name.txt, e.pld_type) l) [] in
      Exp.match_ (evar ak) [Exp.case plhs prhs]
    | Ptype_abstract -> 
      let matchmaker l =
        let mkarg x = "a" ^ string_of_int x in
        let mkparg x = mkp @@ mkarg x in
        let plhs = ptuple (List.mapi (fun i e -> Pat.var @@ mkaststr (mkarg i)) l) in
        let prhs =
          let rec aux i args allargns =
            match args with
            | [] ->
              let a = List.map (fun e -> evar e) @@ List.rev allargns in
              app (evar "@@") [evar "Lwt.return"; tuple a]
            | x :: y -> 
              let argn = mkarg i in
              match x.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Lident n}, _) ->
                let argnp = mkparg i in
                if n = td.ptype_name.txt then 
                  app (evar ">>=") [app (evar "to_adt") [evar argn]; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                else
                  let st = find_some_type tds n in
                  (match st with
                   | Some t ->
                     app (evar ">>=") [kind_mapper t argn; lam (pvar argnp) (aux (i+1) y (argnp :: allargns))]
                   | None -> aux (i+1) y (argn :: allargns))
              | _ -> aux (i+1) y (argn :: allargns) in
          aux 0 l [] in
        Exp.match_ (evar ak) [Exp.case plhs prhs] in 
      (match ctd.ptype_manifest with
       | Some ({ptyp_desc = Ptyp_constr (_, _) } as x) -> matchmaker [x]
       | Some {ptyp_desc = Ptyp_tuple l} -> matchmaker l
       | None -> failwith "[dalify] Open abstract types are not supported"
       | _ -> assert false)
    | Ptype_open -> assert false in
  kind_mapper td "t"

let dali_json_convert madt tds =
  let open Ast_helper in
  let to_json_stris = List.map Derive_ezjsonm.derive_to_json [tds; madt] in
  let of_json_stris = List.map Derive_ezjsonm.derive_of_json [tds; madt] in
  let jsonc_struct = to_json_stris @ of_json_stris in
  Str.module_ @@ Mb.mk (mkaststr "JsonConvert") (Mod.structure jsonc_struct)

let dali_derive tds td dts mn  =
  let template = Template.blob in
  let adt_mod = dali_adt_mod mn in
  let adt_typesig = dali_adt_typesig tds td mn in
  let madt_typedef, madt = dali_madt_typedef tds td mn in
  let json_convert = dali_json_convert madt dts in
  let of_adt = dali_of_adt tds td mn in
  let to_adt = dali_to_adt tds td mn in
  let imodstr_rename = dali_imodstr_rename tds td mn in
  let mmodsig_functs = dali_mmodsig_functs tds td mn in
  let mmodstr_functs = dali_mmodstr_functs tds td mn in
  let mergeable_functs = dali_mergeable_functs tds td mn in
  let mmod_inst = dali_mmod_inst tds td mn in
  let bcsto_inst = dali_bcsto_inst tds td mn in
  let dali_mapper = mk_dali_mapper 
      (adt_mod, adt_typesig, madt_typedef, json_convert, of_adt, to_adt, imodstr_rename, 
      mmodsig_functs, mmodstr_functs, mergeable_functs, mmod_inst, bcsto_inst) in
  dali_mapper.structure dali_mapper (Parse.implementation Versions.ocaml_402 @@ Lexing.from_string template)

let using_dali_mapper () = 
  let mk_dali_derive_mapper mname = {
    default_mapper with 
    structure = fun mapper s ->
      let rec aux s dt = match s with
        | [] -> raise_errorf "[dalify]: Unavailable dalify-ing type."
        | { pstr_desc = Pstr_type tds } :: r ->
          let rec do_dalify td = match td with
            | [] -> aux r dt
            | { ptype_attributes = [({txt = "derive"}, p) ]; } as td :: _ ->
              (match p with
               | PStr [{pstr_desc= Pstr_eval({pexp_desc = Pexp_ident {txt = Longident.Lident "versioned"}}, _); _}] ->
                 dali_derive tds td (List.rev dt) mname
               | PStr [{pstr_desc= Pstr_eval({pexp_desc = Pexp_ident {txt = Longident.Lident "ezjsonm"}}, _); _}] ->
                 aux r (td::dt)
               | _ -> assert false) 
            | x :: r -> do_dalify r in
          do_dalify tds
        | x :: r -> aux r dt in
      default_mapper.structure mapper (aux s [])
  } in
  { default_mapper with 
    structure = fun mapper s ->
      let rec aux s = match s with
        | [] -> []
        | { pstr_desc = Pstr_module 
                {pmb_name; pmb_expr; 
                 pmb_attributes = [({txt = "derive_versioned"}, _) ];} 
          } :: r ->
          let mname = Longident.Lident pmb_name.txt in
          (match pmb_expr.pmod_desc with
           | Pmod_structure x ->
             let dali_derive_mapper = mk_dali_derive_mapper mname in
             s @ (dali_derive_mapper.structure mapper x)
           | _ -> raise_errorf "[dalify]: Invalid module binding.")
        | x :: r -> x :: (aux r) in
      default_mapper.structure mapper (aux s)
  }

let () = Driver.register ~name:"ppx_mtypes" Versions.ocaml_402 (fun _ _ -> using_dali_mapper ())