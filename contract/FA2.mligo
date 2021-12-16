(** 
   This file implement the TZIP-12 protocol for NFT on Tezos
   copyright Wulfman Corporation 2021
*)

type storage = unit

(** transfer entrypoint *)
type atomic_trans = [@layout:comb] {
   to_      : address;
   token_id : nat;
   amount   : nat;
}

type transfer = {
   from_ : address; 
   tx: atomic_trans list
} list

let transfer : transfer -> storage -> operation list * storage = 
   fun (p:transfer) (s:storage) -> ([]: operation list),s

(** balance_of entrypoint *)
type request = {
   owner    : address;
   token_id : nat;
}

(*The layout allow to fix the ordering *)
type callback = [@layout:comb] {
   request : request;
   balance : nat;
}

type balance_of = [@layout:comb] {
   requests : request list;
   callback : callback list contract;
}

let balance_of : balance_of -> storage -> operation list * storage = 
   fun (p: balance_of) (s: storage) -> ([]: operation list),s

(** operator entrypoint *)
type operator = [@layout:comb] {
   owner    : address;
   operator : address;
   token_id : nat; 
}

type update_operators =
   Add_operator    of operator
|  Remove_operator of operator

let update_ops : update_operators -> storage -> operation list * storage = 
   fun (p: update_operators) (s: storage) -> ([]: operation list),s

type parameter = Transfer of transfer | Balance_of of balance_of | Update_operators of update_operators
let main ((p,s):(parameter * storage)) = match p with
   Transfer         p -> transfer   p s
|  Balance_of       p -> balance_of p s
|  Update_operators p -> update_ops p s
