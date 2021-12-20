(** 
   This file implement the TZIP-12 protocol for NFT on Tezos a.k.a FA2
   copyright Wulfman Corporation 2021
*)

(* 
   Errors
*)
module Errors = struct
   let ins_balance     = "FA2_INSUFFICIENT_BALANCE"
   let undefined_token = "FA2_TOKEN_UNDEFINED"
end

(* The type of the storage is not provided in the TZIP-12.
The considaration for designing the storage is to minimize the information stored on chain and the lookup algorithm in the contract.
Possible efficient datastructure are :
   Big_map owner    -> Map (big_map ?) token_id (-> amount if applicable).
   Big_map token_id -> Map (big_map ?) owner    (-> amount if applicable).
*)
module Collection = struct
   type token_id = nat
   type amount_  = nat
   type t = (token_id, amount_) map
   let get_amount_for_token (collection : t) (token_id : token_id) : amount_ = 
      match (Map.find_opt token_id collection) with
         Some (amount_) -> amount_
      |  None           -> 0n

   let increase_amount (tokens : t) (token_id : nat) (amount_ : nat) =
      let current_amount = get_amount_for_token tokens token_id in
      let new_amount     = current_amount + amount_ in
      Map.update token_id (Some new_amount) tokens

   let decrease_amount (tokens : t) (token_id : nat) (amount_ : nat) =
      let current_amount = get_amount_for_token tokens token_id in
      let () = assert_with_error (current_amount >= amount_) Errors.ins_balance in
      let new_amount     = abs(current_amount - amount_) in
      if (new_amount = 0n) then Map.remove token_id tokens
         else Map.update token_id (Some new_amount) tokens 
end
module Ledger = struct
   type owner = address
   type t = (owner, Collection.t) big_map
   
   let get_for_user    (ledger:t) (owner: address) : Collection.t =
      Option.unopt_with_error (Big_map.find_opt owner ledger) Errors.ins_balance

   let update_for_user (ledger:t) (owner: address) (tokens : Collection.t) : t = 
      Big_map.update owner (Some tokens) ledger

   let decrease_token_amount_for_user (ledger : t) (from_ : owner) (token_id : nat) (amount_ : nat) : t = 
      let tokens = get_for_user ledger from_ in
      let tokens = Collection.decrease_amount tokens token_id amount_ in
      let ledger = update_for_user ledger from_ tokens in
      ledger 

   let increase_token_amount_for_user (ledger : t) (to_   : owner) (token_id : nat) (amount_ : nat) : t = 
      let tokens = get_for_user ledger to_ in
      let tokens = Collection.increase_amount tokens token_id amount_ in
      let ledger = update_for_user ledger to_ tokens in
      ledger 
end
module Storage = struct
type token_id = nat
type t = {
   ledger : Ledger.t;
   token_metadata : (token_id, string) big_map;
}

let get_token_for_owner (s:t) (owner : address) = 
   match Big_map.find_opt owner s.ledger with 
      Some (tokens) -> tokens
   |  None          -> Map.empty

let assert_token_exist (s:t) (token_id : nat) : unit  = 
   let _ = Option.unopt_with_error (Big_map.find_opt token_id s.token_metadata)
      Errors.undefined_token in
   ()

let update_ledger (s:t) (ledger:Ledger.t) = {s with ledger = ledger}
end


type storage = Storage.t

(** transfer entrypoint
(list %transfer
  (pair
    (address %from_)
    (list %txs
      (pair
        (address %to_)
        (pair
          (nat %token_id)
          (nat %amount)
        )
      )
    )
  )
)
*)

(*
Each transfer in the batch is specified between one source (from_) address and
a list of destinations. Each transfer_destination specifies token type and the
amount_ to be transferred from the source address to the destination (to_) address.
FA2 does NOT specify an interface for mint and burn operations; however, if an
FA2 token contract implements mint and burn operations, it SHOULD, when possible,
enforce the same logic (core transfer behavior and transfer permission logic)
applied to the token transfer operation. Mint and burn can be considered special
cases of the transfer. Although, it is possible that mint and burn have more or
less restrictive rules than the regular transfer. For instance, mint and burn
operations may be invoked by a special privileged administrative address only.
In this case, regular operator restrictions may not be applicable.
*)


type atomic_trans = [@layout:comb] {
   to_      : address;
   token_id : nat;
   amount   : nat;
}

type transfer_from = {
   from_ : address;
   tx    : atomic_trans list
}
type transfer = transfer_from list

(*
Core Transfer Behavior
FA2 token contracts MUST always implement this behavior.


Every transfer operation MUST happen atomically and in order. If at least one
transfer in the batch cannot be completed, the whole transaction MUST fail, all
token transfers MUST be reverted, and token balances MUST remain unchanged.
-> Modification of storage at the end, common design pattern

Each transfer in the batch MUST decrement token balance of the source (from_)
address by the amount of the transfer and increment token balance of the destination
(to_) address by the amount of the transfer.
-> Expected behavior

If the transfer amount exceeds current token balance of the source address,
the whole transfer operation MUST fail with the error mnemonic "FA2_INSUFFICIENT_BALANCE".
-> adding the error

If the token owner does not hold any tokens of type token_id, the owner's balance
is interpreted as zero. No token owner can have a negative balance.
-> Enforced by data type


The transfer MUST update token balances exactly as the operation
parameters specify it. Transfer operations MUST NOT try to adjust transfer
amounts or try to add/remove additional transfers like transaction fees.

Transfers of zero amount MUST be treated as normal transfers.
-> don't cover corner cases

Transfers with the same address (from_ equals to_) MUST be treated as normal
transfers.

If one of the specified token_ids is not defined within the FA2 contract, the
entrypoint MUST fail with the error mnemonic "FA2_TOKEN_UNDEFINED".
-> need to have a collection of defined token (big_map ?)


Transfer implementations MUST apply transfer permission policy logic (either
default transfer permission policy or
customized one).
If permission logic rejects a transfer, the whole operation MUST fail.
-> permission logic ?


Core transfer behavior MAY be extended. If additional constraints on tokens
transfer are required, FA2 token contract implementation MAY invoke additional
permission policies. If the additional permission fails, the whole transfer
operation MUST fail with a custom error mnemonic.
*)

let transfer : transfer -> storage -> operation list * storage = 
   fun (t:transfer) (s:storage) -> 
   (* This function process the "tx" list. Since all transfer share the same "from_" address, we use a se *)
   let process_atomic_transfer (from_:address) (ledger, t:Ledger.t * atomic_trans) =
      let {to_;token_id;amount=amount_} = t in
      let ()     = Storage.assert_token_exist s token_id in
      let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
      let ledger = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
      ledger
   in
   let process_single_transfer (ledger, t:Ledger.t * transfer_from ) =
      let ledger : Ledger.t = List.fold_left (process_atomic_transfer t.from_) ledger t.tx in
      ledger
   in
   let ledger = List.fold_left process_single_transfer s.ledger t in
   let s = Storage.update_ledger s ledger in
   ([]: operation list),s

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

(*Empty batch is a valid input and MUST be processed as a non-empty one.
For example, an empty transfer batch will not affect token balances, but applicable
transfer core behavior and permission policy MUST be applied. *)

type balance_of = [@layout:comb] {
   requests : request list;
   callback : callback list contract;
}

(* Invocation of the balance_of entrypoint with an empty batch input MUST result in a call to a
callback contract with an empty response batch. *)
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

type parameter = [@layout:comb] | Transfer of transfer | Balance_of of balance_of | Update_operators of update_operators
let main ((p,s):(parameter * storage)) = match p with
   Transfer         p -> transfer   p s
|  Balance_of       p -> balance_of p s
|  Update_operators p -> update_ops p s
