ifndef LIGO
LIGO=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:next
endif

test:
	$(LIGO) run test ./tests/FA2_single_asset.test.mligo
	$(LIGO) run test ./tests/FA2_multi_asset.test.mligo
	$(LIGO) run test ./tests/FA2_nft.test.mligo
	$(LIGO) run test ./with_mint_and_burn_tests/FA2_single_asset.test.mligo
	$(LIGO) run test ./with_mint_and_burn_tests/FA2_multi_asset.test.mligo
	$(LIGO) run test ./with_mint_and_burn_tests/FA2_nft.test.mligo

compile:
	$(LIGO) compile contract ./standard_contract/FA2_single_asset.mligo

check:
	$(LIGO) run test smart_contract/foo.mligo
