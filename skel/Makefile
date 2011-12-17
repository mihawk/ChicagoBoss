ERL=erl
APP={{appid}}
GZIP=./priv/gzip.sh
DEFLATE=./priv/deflate.sh

all:
	$(ERL) -pa {{src}}/ebin -eval 'boss_load:load_all_modules_and_emit_app_file($(APP), "ebin")' -s init stop -noshell

clean:
	rm -fv ebin/*.beam
	rm -fv ebin/$(APP).app
	
update_po:
	$(ERL) -pa ebin -pa ../ChicagoBoss/ebin -eval 'boss_load:load_models("ebin")' -eval 'boss_lang:update_po()' -s init stop -noshell

.PHONY: test
test:
	$(ERL) -pa $(PWD)/ebin -pa {{src}}/ebin -run boss_web_test start $(APP) -noshell

static_gzip:
	$(GZIP)

static_deflate:
	$(DEFLATE)
