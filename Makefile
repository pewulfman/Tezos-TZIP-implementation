test:
	docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next run test contract/test_FA2_single_asset.mligo
	
compile:
	docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next compile contract contract/FA2_single_asset.mligo

check:
	docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next run test contract/foo.mligo