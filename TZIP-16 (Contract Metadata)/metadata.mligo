
type t = (string,bytes) big_map

let metadata = [%bytes
{|{
	"name":"titi"
}|}]

let init : t = Big_map.literal [
	("", [%bytes {|tezos_storage:data|}]);
	("data", metadata);
]
