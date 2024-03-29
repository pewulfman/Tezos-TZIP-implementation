(**
   This file implements the TZIP-7 protocol (a.k.a FA1.2)
   copyright Wulfman Corporation 2022
*)

#import "metadata.mligo" "Metadata"

(*
   Errors
*)
module Errors = struct
   let notEnoughBalance   = "NotEnoughBalance"
   let notEnoughAllowance = "NotEnoughAllowance"
   (* Extra error, see: https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM/edit *)
   let vulnerable_operation = "Switching allowances from N to M is a vulnerability"
end

module Allowance = struct
   type spender        = address
   type allowed_amount = nat
	type t = (spender, allowed_amount) map

   let init () : t = Map.empty

   let get_allowed_amount (a:t) (spender:spender) =
      match Map.find_opt spender a with
         Some v -> v | None -> 0n

   let set_allowed_amount (a:t) (spender:spender) (allowed_amount:allowed_amount) =
      if allowed_amount > 0n then Map.add spender allowed_amount a else Map.remove spender a
end

module Ledger = struct
   type owner      = address
   type spender    = address
   type amount_    = nat
   type t = (owner, amount_ * Allowance.t) big_map

   let init () : t = Big_map.empty

   let get_for_user (ledger:t) (owner: owner) : amount_ * Allowance.t =
      match Big_map.find_opt owner ledger with
         Some (tokens) -> tokens
      |  None          -> 0n,(Map.empty : Allowance.t)

   let update_for_user (ledger:t) (owner: owner) (amount_ : amount_) (allowances : Allowance.t) : t =
      Big_map.update owner (Some (amount_,allowances)) ledger

   let set_approval (ledger:t) (owner: owner) (spender : spender) (allowed_amount: amount_) =
      let tokens,allowances = get_for_user ledger owner in
      let previous_allowances = Allowance.get_allowed_amount allowances spender in
      let () = assert_with_error (previous_allowances = 0n || allowed_amount = 0n) Errors.vulnerable_operation in
      let allowances = Allowance.set_allowed_amount allowances spender allowed_amount in
      let ledger     = update_for_user ledger owner tokens allowances in
      ledger

   let decrease_token_amount_for_user (ledger : t) (spender : spender) (from_ : owner) (amount_ : amount_) : t =
      let tokens,allowances = get_for_user ledger from_ in
      let allowances = if spender = from_ then allowances else
         let allowed_amount = Allowance.get_allowed_amount allowances spender in
         let () = assert_with_error (allowed_amount >= amount_) Errors.notEnoughAllowance in
         let allowed_amount = abs(allowed_amount - amount_) in
         let allowances = Allowance.set_allowed_amount allowances spender allowed_amount in
         allowances
      in
      let () = assert_with_error (tokens >= amount_) Errors.notEnoughBalance in
      let tokens = abs(tokens - amount_) in
      let ledger = update_for_user ledger from_ tokens allowances in
      ledger

   let increase_token_amount_for_user (ledger : t) (to_   : owner) (amount_ : amount_) : t =
      let tokens,allowances = get_for_user ledger to_ in
      let tokens = tokens + amount_ in
      let ledger = update_for_user ledger to_ tokens allowances in
      ledger
end

module TokenMetadata = struct
   type data = {token_id:nat;token_info:(string,bytes)map}
   type t = (nat,data) big_map

   let data = Map.literal [
      ("name", [%bytes {| "Elevate FA1 for test" |}]);
      ("symbol", [%bytes {| "EVA1" |}]);
      ("decimals", [%bytes {| "6" |}]);
   ]

   let init () : t = Big_map.literal [
      (0n, {token_id=0n;token_info=data})
   ]
end

module Storage = struct
   type t = {
      ledger : Ledger.t;
      token_metadata : TokenMetadata.t;
      metadata : Metadata.t;
      totalSupply : nat;
      (* Note: memoizing the sum of all participant balance reduce the cost of getTotalSupply entrypoint.
         However, with this pattern the value has to be manually set at origination which can lead to consistency issues.
      *)
   }
   let init () : t =
      (* let totalSupply = Map.fold (fun acc k (amount,allowes) -> acc + amount) 0n (Ledger.init ()) in *)
      {
         ledger = Ledger.init ();
         token_metadata = TokenMetadata.init ();
         metadata = Metadata.init ();
         totalSupply = 0n;
      }

   let get_amount_for_owner (s:t) (owner : address) =
      let amount_,_ = Ledger.get_for_user s.ledger owner in
      amount_

   let get_allowances_for_owner (s:t) (owner : address) =
      let _,allowances = Ledger.get_for_user s.ledger owner in
      allowances

   let get_ledger (s:t) = s.ledger
   let set_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}

end


type storage = Storage.t


(** transfer entrypoint *)
type transfer = [@layout:comb] {
   from : address ;
   to : address ;
   value : nat;
}
let transfer ({from;to;value}:transfer) (s:storage) =
   let ledger = Storage.get_ledger s in
   let ledger = Ledger.decrease_token_amount_for_user ledger (Tezos.get_sender ()) from value in
   let ledger = Ledger.increase_token_amount_for_user ledger to value in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s

(** approve *)
type approve = [@layout:comb] {
   spender : address;
   value : nat;
}
let approve ({spender;value} : approve) (s:storage) =
   let ledger = Storage.get_ledger s in
   let ledger = Ledger.set_approval ledger (Tezos.get_sender ()) spender value in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s

(** getBalance entrypoint *)
type getAllowance = {owner : address ; spender : address} * nat contract
let getAllowance (({owner;spender},callback): getAllowance) (s: storage) =
   let a = Storage.get_allowances_for_owner s owner in
   let allowed_amount = Allowance.get_allowed_amount a spender in
   let operation = Tezos.transaction allowed_amount 0tez callback in
   ([operation]: operation list),s

(** getBalance entrypoint *)
type getBalance = {owner : address} * nat contract
let getBalance ({owner},callback: getBalance) (s: storage) =
   let balance_ = Storage.get_amount_for_owner s owner in
   let operation = Tezos.transaction balance_ 0tez callback in
   ([operation]: operation list),s

(** getTotalSupply entrypoint *)
type getTotalSupply = unit * nat contract
let getTotalSupply ((),callback : getTotalSupply) (s:storage) =
   let operation = Tezos.transaction s.totalSupply 0tez callback in
   ([operation]: operation list),s

let faucet () (s : storage) =
   let ledger = Storage.get_ledger s in
   let ledger = Ledger.increase_token_amount_for_user ledger (Tezos.get_sender ()) 6_001n in
   let s = Storage.set_ledger s ledger in
   ([]: operation list),s


type parameter =  [@layout:comb]
|  Transfer of transfer
|  Approve of approve
|  Faucet of unit
|  GetAllowance of getAllowance
|  GetBalance of getBalance
|  GetTotalSupply of getTotalSupply

let main ((p,s):(parameter * storage)) = match p with
   Transfer       p -> transfer       p s
|  Approve        p -> approve        p s
|  Faucet         p -> faucet         p s
|  GetAllowance   p -> getAllowance   p s
|  GetBalance     p -> getBalance     p s
|  GetTotalSupply p -> getTotalSupply p s
