(**
   This file implement the TZIP-16 protocol (Contract Metadata) for Tezos
	defined here : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-16/tzip-16.md#how-to-derive-from-tzip-016
   copyright Wulfman Corporation 2022
*)

(*  TZIP-016 : JSON nomenclature
{
	"name":"",                            (* Contract name, recommended *)
	"description":"",                     (* details information *)
	"version":"",                         (* semver version number *)
	"license":{"name":"", "details":""},  (* details is optional *)
	"authors":[],                         (* List of authors with format "Name<'contact'>" *)
	"homepage":"",                        (* URL for human-consumption *)
	"source":{"tools":[], "location":""}, (* High-level language source *)
	"interfaces":[],                      (* TZIP-XYZ others than TZIP-016 *)
	"errors":[],                          (* List of errors translation object : {"error":<michelson>,"expension":<michelson>, "languages":[]} *)
	"views":[]                            (* List of off-chain-view objects *)
} *)


type t = (string,bytes) big_map

let metadata = [%bytes
{|{
	"name":"WULF FA1.2",
	"description":"Example FA1.2 implementation",
	"version":"0.1.0",
	"license":{"name":"MIT"},
	"authors":["Pierre-Emmanuel Wulfman<pierre-emmanuel@wulfman.fr>"],
	"homepage":"",
	"source":{"tools":["Ligo"], "location":"https://github.com/pewulfman/Tezos-TZIP-implementation/blob/main/TZIP-7%20(FA1.2)%20/FA1.2.mligo"},
	"interfaces":["TZIP-007, TZIP-012"],
	"errors":[],
	"views":[]

}|}]

let init () : t = Big_map.literal [
	("", [%bytes {|tezos-storage:data|}]);
	("data", metadata);
]
