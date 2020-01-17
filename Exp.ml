open Abr;;
open Acl;;
open Acm;;
open AcmC;;




(***************************************************************************)

(*calcul de la taille memoire prise par une structure de données en nombre de mots*)
let sizeof v =
  let rec rec_size d r =
    if List.memq r d then (1, d) else
    if not(Obj.is_block r) then (1, r::d) else
    if (Obj.tag r) = (Obj.double_tag) then (2, r::d) else
    if (Obj.tag r) = (Obj.string_tag) then (Obj.size r, r::d) else
    if (Obj.tag r) = (Obj.object_tag) ||
       (Obj.tag r) = (Obj.closure_tag)
    then invalid_arg "please only provide datas"
    else
      let len = Obj.size r in
      let rec aux d sum i =
        if i >= len then (sum, r::d) else
        let this = Obj.field r i in
        let this_size, d = rec_size d this in
        aux d (sum + this_size) (i+1)
      in
      aux d (1) 0
  in
  rec_size [] (Obj.repr v);;


(*
let intl=[4;2;3;8;1;9;6;7;5] in
let abr=(list_to_abr intl Empty_abr) in
let acm= comp_abr_acm abr in
let acl= comp_abr_acl abr in
let acmC=comp_abr_acmC abr in(  
        cpt_reset();
        test_acl intl abr;
        cpt_reset();
        test_acm intl abr;
        cpt_reset();
        test_acmC intl abr;
Printf.printf "abr = %d\n" (match (sizeof abr) with | (a,l) -> a);
Printf.printf "acl = %d\n" (match (sizeof acl) with | (a,l) -> a);
Printf.printf "acm = %d\n" (match (sizeof acm) with | (a,l) -> a);
Printf.printf "acmC = %d\n" (match (sizeof acmC) with | (a,l) -> a)
)
*)
(***************************************************************************)
(* calcul le temps moyen de la recherche d'une valeur dans les différentes structures de données*)

let av_srch_time intl nb_it =
    let abr=(list_to_abr intl Empty_abr) in
    let acl=(comp_abr_acl abr) in
    let acm=(comp_abr_acm abr) in
    let acmC=(comp_abr_acmC abr) in
    let rec srch cpt l=
        if(cpt<=0) then l
        else 
        let v=((Random.int (List.length intl))+1) in
        let t=Sys.time () in
        ignore (search_val_in_abr v abr);
        let vabr=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acl v acl);
        let vacl=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acm v acm);
        let vacm=((Sys.time ()) -. t) in
        let t=Sys.time () in
        ignore (search_val_in_acmC v acmC);
        let vacmC=((Sys.time ()) -. t) in(
            match l with 
            | [] -> srch (cpt-1) [(vabr,vacl,vacm,vacmC)] 
            | (v1,v2,v3,v4)::[] -> srch (cpt-1) [(v1 +. vabr),(v2 +. vacl),(v3 +. vacm),(v4 +. vacmC)]
            | _ -> assert false
        )
    in
        match(srch nb_it []) with 
            | (v1,v2,v3,v4)::[] -> [((List.length intl),(v1 /. 4.),(v2 /. 4.),(v3 /. 4.),(v4 /. 4.))]
            | _ -> assert false ;;

let time_test intl outf nb_it=
    let rec tt intl l nb_it=
        match intl with 
        | [] -> l
        | h::t -> tt t (l@(av_srch_time h nb_it)) nb_it
    in
        let wf= open_out outf in
        Printf.fprintf wf "  nb_noeuds tmps_abr  tmps_acl  tmps_acm  tmps_acmC\n";
        let l=(tt intl [] nb_it) in
            let rec write_list l =
                match l with 
                | [] -> close_out wf; 0
                | (n,v1,v2,v3,v4)::t ->  Printf.fprintf wf "   %d    %f  %f  %f  %f  \n" n v1 v2 v3 v4; write_list t
            in
                write_list l;;
    
 
(**  AJOUT*************************************************** 
	CALCULE MEMOIRE
*)
let av_comp_mem intl outf =
    Gc.compact();
    let init_all = Gc.allocated_bytes() in(
      let abr = (list_to_abr  (gen_permutation intl)  Empty_abr ) in (
        Gc.compact();
        let abr_all = Gc.allocated_bytes() in(
          let growth_abr = (abr_all -. init_all) in(
         (** Printf.printf "ABR SIMPLE     ==> %f \n" growth_abr;*)
          Printf.fprintf outf " %d "  intl ; 

          Printf.fprintf outf " %f "  growth_abr ; 
          let acl = (comp_abr_acl abr ) in (
          Gc.compact();            
          let acl_all = Gc.allocated_bytes() in(
          let growth_acl = (acl_all -. abr_all)in(
         (** Printf.printf "ABR COMPRESSE ==> %f \n" growth_acl;*)
           Printf.fprintf outf " %f "  growth_acl ;
          
          let acm=(comp_abr_acm abr) in(
          Gc.compact();
           let acm_all = Gc.allocated_bytes() in (
           let growth_acm = (acm_all -. acl_all)in(
           (**Printf.printf "ABR COMPRESSE MAP ==> %f \n" growth_acm ;*)
            Printf.fprintf outf " %f "  growth_acm ; 
           let acmC=(comp_abr_acmC abr )in(
           Gc.compact();
           let all_acmC = Gc.allocated_bytes () in (
           let acmC_growth = (all_acmC -. acm_all) in (
           
           (**Printf.printf "ABR COMPRESSE comp == > %f \n"  acmC_growth1 ;*)
            Printf.fprintf outf " %f \n"  acmC_growth ; 
  
      )   )  ) )
      )   )   ) ))  )  ) )
)   ;;
let nbr_mem intl outf =
 let wf = open_out outf in (
    Printf.fprintf wf "nb_Cle     mem_abr    mem_acl    mem_acm     mem_acmC \n";
    let rec build l wf =
        match l with 
        | [] -> ""
        | h :: t ->  av_comp_mem h wf;  build t wf
    in
           
    build intl wf 
           
    );;

             
(***************************************************************************)
(* calcul du nombre de noeuds et d'ids crées quand on compresse un abr*)
let rec r_in_l r l =
    match l with
    | [] -> false
    | h::t -> if(r=h) then true else r_in_l r t;;
    
let nbr_noeuds_acl acl = 
    let rec nn acl rl =
        match acl with 
        | Empty_acl -> 0
        | Node_acl(nl,fg,fd) -> if (r_in_l (ref (acl)) (!rl)) then 0
                                else
                                    (rl:=((ref (acl))::(!rl));
                                    (1+(nn !fg rl)+(nn !fd rl)))
    in
        nn acl (ref []);;
    
let nbr_clefs_acl acl = 
    let rec nc acl rl=
        match acl with 
        | Empty_acl -> 0
        | Node_acl(nl,fg,fd) -> if (r_in_l (ref (acl)) (!rl)) then 0
                                else
                                    (rl:=((ref (acl))::(!rl));
                                    ((List.length (nl.vals))+(nc !fg rl)+(nc !fd rl)))
    in
        nc acl (ref []);;



let rec nbid l =
    match l with
    | [] -> 0
    | (v,idl)::t -> (List.length idl)+(nbid t)

let nbr_nci_acl abr =
    let rec rn acl rl =
        match acl with 
        | Empty_acl -> (0,0,0)
        | Node_acl(nl,fg,fd) ->
            if (r_in_l (ref (acl)) (!rl)) then (0,0,0)
            else
                (rl:=((ref (acl))::(!rl));
                let v1,v2,v3 = (rn !fg rl) in
                let v4,v5,v6 = (rn !fd rl) in
                    (v1+v4+1,v2+v5+(List.length nl.vals),v3+v6+(nbid nl.vals)+(List.length nl.a_fg)+(List.length nl.a_fd)))
    in
        rn (comp_abr_acl abr) (ref []);;
        
let rec count_binding bl x y=
    match bl with
    | [] -> (y,x)
    | (idl,v)::t -> count_binding t (x+(List.length idl)) (y+1);;  

  
let nbr_nci_acm abr =
(*  nombre de noeuds, nombre de valeurs , nombre d'identifiants *)
    let rec rn acm rl =
        match acm with 
        | Empty_acm -> (0,0,0)
        | Node_acm(n,fg,fd) ->
            if (r_in_l (ref (acm)) (!rl)) then (0,0,0)
            else
                (rl:=((ref (acm))::(!rl));
                let v1,v2,v3 = (rn !fg rl) in
                let v4,v5,v6 = (rn !fd rl) in
                let vs,ids = (count_binding (NodeMap.bindings n.vals) 0 0 ) in
                    (v1+v4+1,v2+v5+vs,v3+v6+ids+(List.length n.a_fg)+(List.length n.a_fd)))
    in
        rn (comp_abr_acm abr) (ref []);;   
        
      
let nbr_nci_acl_to_file intl outf=
    let rec build intl l = 
        match intl with 
        | [] -> l
        | h::t -> let v1,v2,v3 =(nbr_nci_acl (list_to_abr h Empty_abr) )  in
            ( (List.length h) ,v1,v2,v3)::(build t l)      
    in 
        let wf= (open_out outf) in
        Printf.fprintf wf "nb_valeurs  nb_noeuds  nb_cles  nb_ids\n";
        let rec write_list l =
            match l with 
            | [] -> close_out wf; 0
            | (h,v1,v2,v3)::t -> 
                Printf.fprintf wf "  %d  %d  %d  %d  \n" h v1 v2 v3; write_list t
        in 
            write_list (build intl []);;
 


       
let nbr_nci_acm_to_file intl outf=
    let rec build intl l = 
        match intl with 
        | [] -> l
        | h::t -> let v1,v2,v3 =(nbr_nci_acm (list_to_abr h Empty_abr))  in
            ((List.length h),v1,v2,v3)::(build t l)      
    in 
        let wf= (open_out outf) in
        Printf.fprintf wf "nb_valeurs  nb_noeuds  nb_cles  nb_ids\n";
        let rec write_list l =
            match l with 
            | [] -> close_out wf; 0
            | (h,v1,v2,v3)::t -> 
                Printf.fprintf wf "  %d  %d  %d  %d  \n" h v1 v2 v3; write_list t
        in 
            write_list (build intl []);;

  
  
let rec count_binding_inn bl x y=
    match bl with
    | [] -> (x,y)
    | (idl,(a,b))::t -> count_binding_inn t (x+(List.length a)+(List.length idl)) (y+1);;
let nbr_nci_acCInner ac refl =
    let rec rnni ac refl =
        match !ac with 
        | Empty_acmCInner -> (0,0,0)
        | Node_acmCInner(n,rfg,rfd) ->
            if ( (r_in_l ac !refl)=true ) then (0,0,0) 
            else(
                refl:=( ac::(!refl) ); 
                let v1,v2,v3 =(rnni rfg refl) in
                let v4,v5,v6 =(rnni rfd refl) in
                let ids,vvs=(count_binding_inn (NodeMap.bindings (n.valsCInner)) 0 0) in
                (v1+v4+1, v2+v5+vvs, v3+v6+ids+(List.length n.a_fg)+(List.length n.a_fd))
            )
    in
        rnni ac refl;;
        
let nbr_nci_acmC abr =
    let rec rnno ac rlo rli =
        match !ac with 
        | Empty_acmCOuter -> (0,0,0,0)
        | Node_acmCOuter(n,rfg,rfd) ->
            if ( (r_in_l ac !rlo) = true) then (0,0,0,0) 
            else(
                rlo:=( ac::(!rlo) );
                let v1,v2,v3,v4 =(rnno rfg rlo rli) in
                let v5,v6,v7,v8 =(rnno rfd rlo rli) in    
                let x,y,z=(nbr_nci_acCInner (ref n.valsCOuter) rli) in
                (v1+v5+1,v2+v6+x, v3+v7+y, v4+v8+z+(List.length n.a_fg)+(List.length n.a_fd))
            )
    in
        rnno (ref(comp_abr_acmC abr)) (ref []) (ref []);;
      


  
let nbr_nci_acmC_to_file intl outf=
    let rec build intl l = 
        match intl with 
        | [] -> l
        | h::t -> let v1,v2,v3,v4 =(nbr_nci_acmC ( (list_to_abr  h Empty_abr)) )  in
            ((List.length h),v1,v2,v3,v4)::(build t l)      
    in 
        let wf= (open_out outf)  in
        Printf.fprintf wf "nb_valeurs noeuds-externes noeuds-internes nb_cles nb_ids\n";
        let rec write_list l =
            match l with 
            | [] -> close_out wf; 0
            | (h,v1,v2,v3,v4)::t -> 
                Printf.fprintf wf "  %d  %d  %d  %d  %d   \n" h v1 v2 v3 v4; write_list t
        in 
            write_list (build intl []);;      
        

let cles_par_noeuds intl outf=    
    let rec build intl l = 
        match intl with 
        | [] -> l
        | h::t -> 
            let abr=(list_to_abr h Empty_abr) in
            let a1,b1,c1=(nbr_nci_acl abr) in 
            let a2,b2,c2,d2=(nbr_nci_acmC abr) in
            (List.length h, (float_of_int b1) /. (float_of_int a1), (float_of_int c1) /. (float_of_int b1), (float_of_int c2) /. (float_of_int b2), (float_of_int d2) /. (float_of_int c2)  )::(build t l)
    in 
        let wf= (open_out outf) in
        Printf.fprintf wf "nb_valeurs  nds_acl_acm    ids_acl_acm  nds_acmc  ids_acmc\n";
        let rec write_list l =
            match l with 
            | [] -> close_out wf; 0
            | (h,v1,v2,v3,v4)::t -> 
                Printf.fprintf wf "  %d    %f    %f    %f    %f \n" h v1 v2 v3 v4; write_list t
        in 
            write_list (build intl []);;
 
        
        
  
let file_str_to_intl filename=
    let st=(input_line (open_in filename)) in
    let strl=(String.split_on_char ',' (String.sub st 1 ((String.length st)-2))) in
    let rec rrf sl il b=
        match sl with 
        | [] -> il
        | h::t -> if(b=true) then  rrf t (il@[(int_of_string h)])  false
                    else rrf t (il@[(int_of_string ( String.sub h 1 ((String.length h)-1)  ))]) false
    in
        rrf strl [] true ;;


let rec test_files l =
    match l with 
    | [] -> Printf.printf ""
    | h::t -> 
        let l= file_str_to_intl h in
        let acl=comp_abr_acl (list_to_abr l Empty_abr) in
        let nbclefs=(nbr_clefs_acl acl) in
        let nbnoeuds=(nbr_noeuds_acl acl) in(
            Printf.printf "Fichier : %s\n" h;
            Printf.printf "nbr total noeuds : %d\n" nbnoeuds;
            Printf.printf "nbr tatal clefs : %d\n" nbclefs;
            Printf.printf "nbr moyen de clefs par noeuds : %f\n\n" ((float_of_int nbclefs) /. (float_of_int nbnoeuds))
        ); test_files t;;  



test_files ["Jeu_de_tests/donnee10000.txt"]
  
(*        

["Jeu_de_tests/donnee10000.txt"]



["Jeu_de_tests/donnee50000.txt";"Jeu_de_tests/donnee100.txt";"Jeu_de_tests/donnee150.txt";"Jeu_de_tests/donnee500.txt";"Jeu_de_tests/donnee750.txt"]  

let l=List.map gen_permutation [1000;1500;2000;2500;3000;3500;4000;4500;5000];;
time_test l  "output.txt" 10;;
nbr_nci_acl_to_file l "nciacl.txt";;
nbr_nci_acm_to_file l "nciacm.txt";;
nbr_nci_acmC_to_file l "nciacmc.txt";;

let l=List.map gen_permutation [1000;1500;2000;2500];;
          cles_par_noeuds l "pers.txt";;


let l=List.map gen_permutation [1000;1500;2000;2500;3000;3500;4000;4500;5000];;
          cles_par_noeuds l "pers.txt";;



  
let a=add_cpl_abr [] 1 Empty_abr;;  
let a=add_cpl_abr [1] 3 a;;
let a=add_cpl_abr [2] 5 a;;
let a=add_cpl_abr [2;1] 7 a;;
let a=add_cpl_abr [3] 9 a;;
Printf.printf "%s\n" (abr_tostr a);;
let a=comp_abr_acmCInner a;;
Printf.printf "%s\n"  (to_str_smcI a);;
let x,y,z=(nbr_nci_acCInner a);;
Printf.printf "noeuds=%d  clés=%d  ids=%d\n"  x y z;;
 
  
let a=add_cpl_abr [] 2 Empty_abr;;  
let a=add_cpl_abr [2] 6 a;;
Printf.printf "%s\n" (abr_tostr a);;
let a=comp_abr_acmCInner a;;
Printf.printf "%s\n"  (to_str_smcI a);;
let x,y,z=(nbr_nci_acCInner a);;
Printf.printf "noeuds=%d  clés=%d  ids=%d\n"  x y z;;
 
 
 
 
 

let a=NodeMap.empty in
Printf.printf ">%s\n"  (clpl_tostr (NodeMap.bindings a));
let a=NodeMap.add []  ([],1) a in
Printf.printf ">%s\n"  (clpl_tostr (NodeMap.bindings a));
let a=NodeMap.add [1]  ([2],5) a in
Printf.printf ">%s\n"  (clpl_tostr (NodeMap.bindings a));
let a=NodeMap.add [2]  ([3],9) a in  
Printf.printf "%d %d\n" (comp_ids [2] [1]) (comp_ids [2] []);

Printf.printf ">%s\n"  (clpl_tostr (NodeMap.bindings a));;



*







    cpt_reset();;
    let intl=[4;2;8;1;3;6;9;5;7] in
    let abr = (list_to_abr intl Empty_abr) in
    test_acmC intl abr;;

        let l=[[4;2;8;1;3;6;9;5;7]];;
        nbr_nci_acmC_to_file l "nciacmc.txt";;



let l=List.map gen_permutation [100;150;200;250;300;350;400;450;500];;
  nbr_nci_acmC_to_file l "nciacmc.txt";;
    cpt_reset();;
    let intl=[4;2;8;1;3;6;9;5;7] in
    let abr = (list_to_abr intl Empty_abr) in
    test_acmC intl abr;;



test_files ["Jeu_de_tests/donnee50000.txt"];"Jeu_de_tests/donnee100.txt";"Jeu_de_tests/donnee150.txt";"Jeu_de_tests/donnee500.txt";"Jeu_de_tests/donnee750.txt";"Jeu_de_tests/donnee1000.txt";"Jeu_de_tests/donnee10000.txt"]   *) 


(* let nbr_nci_acC abr  nombre de noeuds de clées et d'identifiants dans un acmC pour un abr quelqconque   *)   
        
    (*    
        
let l=List.map gen_permutation [100;150;200;250;300;350;400;450;500];;

time_test l  "output.txt" 10;;
nbr_nci_acl_to_file l "nciacl.txt";;
nbr_nci_acm_to_file l "nciacm.txt";;
nbr_nci_acmC_to_file l "nciacmc.txt";;


let file_str_to_intl filename=
    let st=(input_line (open_in filename)) in
    let strl=(String.split_on_char ',' (String.sub st 1 ((String.length st)-2))) in
    let rec rrf sl il b=
        match sl with 
        | [] -> il
        | h::t -> if(b=true) then  rrf t (il@[(int_of_string h)])  false
                    else rrf t (il@[(int_of_string ( String.sub h 1 ((String.length h)-1)  ))]) false
    in
        rrf strl [] true ;;


let rec test_files l =
    match l with 
    | [] -> Printf.printf ""
    | h::t -> 
        let l= file_str_to_intl h in
        let acl=comp_abr_acl (list_to_abr l Empty_abr) in
        let nbclefs=(nbr_clefs_acl acl) in
        let nbnoeuds=(nbr_noeuds_acl acl) in(
            Printf.printf "Fichier : %s\n" h;
            Printf.printf "nbr total noeuds : %d\n" nbnoeuds;
            Printf.printf "nbr tatal clefs : %d\n" nbclefs;
            Printf.printf "nbr moyen de clefs par noeuds : %d\n\n" (nbclefs/nbnoeuds)
        ); test_files t;;   *)
        
(*test_files ["Jeu_de_tests/donnee100.txt"]        *)
        
        
        
