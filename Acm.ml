open Abr;;
open Acl;;

module Cell =
    struct
        type t = int list
        let compare l1 l2 = comp_ids l1 l2
    end
module NodeMap = Map.Make(Cell)


type acm = Empty_acm
             | Node_acm of (n_acm * acm ref * acm ref)
and n_acm ={
    mutable vals: int NodeMap.t;
    mutable a_fg: int list;
    mutable a_fd: int list}
type strs_acm={
    mutable str_list: str_acm list}
and str_acm={
    str:string;
    mutable p: acm ref }
    
let deja_vu_acm s strs =
  let rec rec_deja_vu str l =
    match l with
      | [] -> (ref Empty_acm,false)
      | h::t -> if((compare s h.str)==0) then (h.p,true) else rec_deja_vu s t
  in
    rec_deja_vu s strs.str_list
    
let add_str_acm s strs node =
  strs.str_list <- (strs.str_list)@[{str=s;p=node}];strs    
  
let comb_abr_acm abr racm et_l=
    let rec rec_combine abr racm et_l =
        match(!racm,abr) with
        | (Node_acm(nacm,rfg,rfd),Node_abr(v,abrfg,abrfd)) ->(
            let et_l_fg = et_l@(nacm.a_fg) in
            let et_l_fd = et_l@(nacm.a_fd) in 
                ignore (rec_combine abrfg rfg et_l_fg);
                ignore (rec_combine abrfd rfd et_l_fd);
                nacm.vals <- (NodeMap.add et_l v (nacm.vals));
                racm)
        | _ -> ref Empty_acm
  in
    rec_combine abr racm et_l

let comp_abr_acm abr=
  let rec rec_compress abr str_l =
    let str=struct_abr abr in
    let tup=(deja_vu_acm str str_l) in
      match tup with
        | (x,true) ->(
            match (!x) with
            | Node_acm(_,_,_) ->
                let new_id=( gen_id ()) in
                    ((comb_abr_acm abr x [new_id]),[new_id])
            | _ -> assert false)
        | (x,false) ->
            match (!x,abr) with 
            | (Empty_acm,Node_abr (abrv,abrfg,abrfd)) ->(
                let (rfg,idsfg)= (rec_compress abrfg str_l) in
                let (rfd,idsfd)= (rec_compress abrfd str_l) in    
                let ref_nn=(ref (Node_acm(
                        {vals=( NodeMap.add [] abrv (NodeMap.empty) );
                        a_fg=idsfg;
                        a_fd=idsfd},rfg,rfd))) in
                    ignore (add_str_acm str str_l ref_nn);
                    (ref_nn,[]))
            | (Empty_acm,Empty_abr) -> (ref Empty_acm,[])
            | _ -> assert false
    in
        match (rec_compress abr  ({str_list=[]})) with (x,y) -> !x ;;
  

let search_val_in_acm v node =
    let rec srch v et_l node =
        match (node) with 
        | Empty_acm -> assert false
        | Node_acm(nacm,fg,fd)-> 
            let new_v=(NodeMap.find et_l (nacm.vals)) in
            if(v=new_v) then
                (v,et_l)
            else(
                if(v>new_v) then
                    srch v (et_l@(nacm.a_fd)) (!fd)
                else
                    srch v (et_l@(nacm.a_fg)) (!fg)   
            )
    in
        srch v [] node            

