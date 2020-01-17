open Abr;;
open Acl;;
open Acm;;
open AcmC;;
open Exp;;


(* Tests de validité *)
let rec str_srch_acl vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acl h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acl t tr)
        )
    | [] -> "";;    
                                   
                           
let test_acl intl abr= 
        Printf.printf "%s\n" (str_srch_acl intl (comp_abr_acl abr));;
        
let rec str_srch_acm vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acm h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acm t tr)
        )
    | [] -> "";;    
                                   
                           
let test_acm intl abr= 
        Printf.printf "%s\n" (str_srch_acm intl (comp_abr_acm abr));;
        

let rec str_srch_acm vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acm h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acm t tr)
        )
    | [] -> "";;    
                                   
                           
let test_acm intl abr= 
        Printf.printf "%s\n" (str_srch_acm intl (comp_abr_acm abr));;
  
let rec str_srch_acmC vals tr =
    match vals with 
    | h::t ->(
        match (search_val_in_acmC h tr) with
        |(v,l) -> 
            "("^(string_of_int v)^","^(intl_to_str l)^")\n"^(str_srch_acmC t tr)
        )
    | [] -> "";;    
                                   
                           
let test_acmC intl abr= 
        Printf.printf "%s\n" (str_srch_acmC intl (comp_abr_acmC abr));;


   
let full_test n = 
    let intl=(gen_permutation n) in
    let abr = (list_to_abr intl Empty_abr) in(
        test_acl intl abr;
        cpt_reset();
        test_acm intl abr;
        cpt_reset();
        test_acmC intl abr
    );;
    
(* Tests expérimentaux *)

(* mettre ici les instrcutions utilisées pour obtenir les réusltats présentés dans les graphes*)
    
