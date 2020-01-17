open Abr;;
open Acl;;
open Acm;;


type acmCInner = Empty_acmCInner
             | Node_acmCInner of (n_acmCInner * acmCInner ref * acmCInner ref)
and n_acmCInner ={
    mutable valsCInner: (int list * int) NodeMap.t;
    mutable a_fg: int list;
    mutable a_fd: int list}
  
type acmCOuter = Empty_acmCOuter
             | Node_acmCOuter of (n_acmCOuter * acmCOuter ref * acmCOuter ref)
and n_acmCOuter ={
    mutable valsCOuter: acmCInner;
    mutable a_fg: int list;
    mutable a_fd: int list}  
  
    
type strs_acmCInner={
    mutable str_listC: str_acmCInner list}
and str_acmCInner={
    strC:string;
    mutable p: acmCInner ref }
    
let deja_vu_acmCInner s strs =
  let rec rec_deja_vu str l =
    match l with
      | [] -> (ref Empty_acmCInner,false)
      | h::t -> if((compare s h.strC)==0) then (h.p,true) else rec_deja_vu s t
  in
    rec_deja_vu s strs.str_listC
    
let add_str_acmCInner s strs node =
  strs.str_listC <- (strs.str_listC)@[{strC=s;p=node}];strs        

let rec add_cpl_abr et_l v abr =
    match abr with 
    | Empty_abr -> 
        Node_abr((et_l,v), Empty_abr, Empty_abr)
    | Node_abr((l,n),fg,fd) ->
        if((comp_ids et_l l)=1) 
        then 
            let nfd= (add_cpl_abr et_l v fd) in
            equi (Node_abr((l,n),fg,nfd))
        else 
            let nfg= (add_cpl_abr et_l v fd) in
            equi (Node_abr((l,n),nfg,fd))

let map_to_abr m =
    NodeMap.fold add_cpl_abr m Empty_abr;;

let rec clpl_tostr l =
    match l with
    | [] -> ""
    | (idlo1,(idli1,v1))::(idlo2,(idli2,v2))::t -> 
        "("^(intl_to_str idlo1)^"("^(string_of_int v1)^","^(intl_to_str idli1)^"));"^"("^(intl_to_str idlo2)^"("^(string_of_int v2)^","^(intl_to_str idli2)^"));"^(clpl_tostr t)
    | (idlo,(idli,v))::t ->  "("^(intl_to_str idlo)^"("^(string_of_int v)^","^(intl_to_str idli)^"))" ^(clpl_tostr t)

let to_str_smcI acI=
    let rec tss aci =
    match aci with
    | Empty_acmCInner -> "@"
    | Node_acmCInner(n,rfg,rfd) -> 
            let lcp =(NodeMap.bindings n.valsCInner) in
            let stn=(clpl_tostr lcp) in  
                let st=
                ("vI:["^stn^"]" 
                ^" \nfgI="^
                    (if((List.length n.a_fg)>0) then string_of_int(List.nth n.a_fg 0) else "@")
                ^" \nfdI="^
                    ( if((List.length n.a_fd)>0) then string_of_int(List.nth n.a_fd 0)else "@")^"\n"^">fg\n"^(tss !rfg)^"\n>fd\n"^(tss !rfd)
                )
                in st

    in
        tss acI

let comb_abr_acmCInner abr racm et_l=
    let rec rec_combine abr racm et_l =
        match(!racm,abr) with
        | (Node_acmCInner(nacm,rfg,rfd),Node_abr((v,vb),abrfg,abrfd)) ->(
            let et_l_fg = et_l@(nacm.a_fg) in
            let et_l_fd = et_l@(nacm.a_fd) in 
                ignore (rec_combine abrfg rfg et_l_fg);
                ignore (rec_combine abrfd rfd et_l_fd);
                nacm.valsCInner <- (NodeMap.add et_l (v,vb) (nacm.valsCInner));
                racm)
        | _ -> ref Empty_acmCInner
  in
    rec_combine abr racm et_l

let comp_abr_acmCInner abr=
  let rec rec_compress abr str_l=
    let str=struct_abr abr in
    let tup=(deja_vu_acmCInner str str_l) in
      match tup with
        | (x,true) ->(
            match (!x) with
            | Node_acmCInner(_,_,_) ->
                let new_id=( gen_id ()) in
                    ((comb_abr_acmCInner abr x [new_id]),[new_id])
            | _ -> assert false)
        | (x,false) ->
            match (!x,abr) with 
            | (Empty_acmCInner,Node_abr (abrv,abrfg,abrfd)) ->(
                let (rfg,idsfg)= (rec_compress abrfg str_l) in
                let (rfd,idsfd)= (rec_compress abrfd str_l) in                  
                let ref_nn =( ref (Node_acmCInner(
                        {valsCInner=(NodeMap.add [] abrv (NodeMap.empty));
                        a_fg=idsfg;
                        a_fd=idsfd},rfg,rfd))) in
                ignore (add_str_acmCInner str str_l ref_nn);
                (ref_nn,[]))
            | (Empty_acmCInner,Empty_abr) -> (ref Empty_acmCInner,[])
            | _ -> assert false
    in
        match (rec_compress abr ({str_listC=[]})) with (ra,l) -> !ra;;


(* (ref acm, ref acmc) rl *)
 
let add_rcp_to_rcpl racm racmc rl =
    rl:=((racm,racmc)::!rl)
 
let rec racm_in_rcpl racm  rl =
    match rl with 
    | [] -> (ref Empty_acmCOuter,false)
    | (r1,r2)::t -> if (r1=racm) then (r2,true) else racm_in_rcpl racm t
 
let rec conv_acm_acmCOuter racm rl= 
    let (r,b) = (racm_in_rcpl racm !rl) in
    if(b) then 
        r
    else
        match !racm with
        | Empty_acm -> ref Empty_acmCOuter
        | Node_acm(n,fg,fd) -> 
                            let abr=(map_to_abr n.vals) in
                            let rnn=ref (Node_acmCOuter({valsCOuter=(comp_abr_acmCInner abr); 
                                a_fg=n.a_fg;
                                a_fd=n.a_fd},
                                conv_acm_acmCOuter fg rl,
                                conv_acm_acmCOuter fd rl)) in
                                add_rcp_to_rcpl racm rnn rl;
                                rnn
            


let comp_abr_acmC abr =
    !(conv_acm_acmCOuter (ref (comp_abr_acm abr)) (ref []))


let search_val_in_acmCInner v et_lO node =
    let rec srch v et_lO et_lI node =
        match node with 
        | Empty_acmCInner -> assert false 
        | Node_acmCInner(n,fg,fd) ->
            let f=(NodeMap.find et_lI (n.valsCInner)) in
                match f with 
                | (etl,ent) -> 
                    if ((comp_ids et_lO etl) = 1) then
                        if(!fd != Empty_acmCInner) then
                            srch v et_lO (et_lI@n.a_fd) !fd
                        else
                            ((v,et_lI),1)
                    else
                        if((comp_ids et_lO etl) = (-1)) then
                            if(!fg != Empty_acmCInner) then
                                srch v et_lO (et_lI@n.a_fg) !fg
                            else
                                ((v,et_lI),-1)
                        else
                            if(v>ent) then 
                               ((v,et_lI),1)                         
                            else
                            if (v=ent) then
                                ((v,et_lI),0)
                            else
                                ((v,et_lI),-1)
    in 
        srch v et_lO [] node

let search_val_in_acmC v node =
    let rec srch v et_lO node d=
        match node with 
        | Empty_acmCOuter ->assert false
        | Node_acmCOuter(n,fg,fd) ->
            let f=(search_val_in_acmCInner v et_lO (n.valsCOuter)) in
                match f with 
                | ((ent,etl),r) ->(
                    match r with 
                    | 0 -> (ent,et_lO)
                    | 1 -> srch v (et_lO@n.a_fd) !fd "d"
                    | -1 -> srch v (et_lO@n.a_fg) !fg "g"
                    | _ -> assert false
                )
                
    in 
        srch v [] node "r";;

