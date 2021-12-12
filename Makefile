include config.mk

all : inigo
.PHONY : bootstrap server idrall inigo install static local-server deploy-init deploy test clean distclean golden

define build_base_dep
	(cd Base/$(1) && idris2 --build Bootstrap.ipkg --build-dir ../../build)
endef

bootstrap :
	mkdir -p build
	$(call build_base_dep,Color)
	$(call build_base_dep,Extra)
	$(call build_base_dep,Fmt)
	$(call build_base_dep,IdrTest)
	$(call build_base_dep,Markdown)
	$(call build_base_dep,SemVar)
	$(call build_base_dep,Toml)
	$(MAKE) inigo

server :
	idris2 --build Server/InigoServer.ipkg --cg node

idrall :
	idris2 --build idrall/idrall.ipkg
	mkdir -p depends/idrall-0
	cp -r idrall/build/ttc/* depends/idrall-0/

inigo : idrall
	idris2 --build Inigo.ipkg
	# echo '#!/usr/bin/env node' | cat - build/exec/inigo > temp && mv temp build/exec/inigo
	# chmod +x build/exec/inigo
	@echo "Built \"build/exec/inigo\""

install : inigo
	rm -r $(INSTALL_DIR)/inigo_app
	cp build/exec/inigo $(INSTALL_DIR)
	cp -r build/exec/inigo_app $(INSTALL_DIR)/inigo_app
	chmod +x $(INSTALL_DIR)/inigo

static :
	env SKIP_EXT=true node Server/InigoStatic/localize.js Server/InigoStatic/Pages Server/InigoStatic/Local/pages.json
	node Server/InigoStatic/localize.js Server/InigoStatic/Static Server/InigoStatic/Local/static.json

local-server : server static
	cloudworker --debug \
		--kv-file "pages=./Server/InigoStatic/Local/pages.json" \
		--kv-file "static=./Server/InigoStatic/Local/static.json" \
		--kv-file "index=./Server/InigoStatic/Local/index.json" \
		--kv-file "packages=./Server/InigoStatic/Local/packages.json" \
		--kv-file "deps=./Server/InigoStatic/Local/deps.json" \
		--kv-file "archives=./Server/InigoStatic/Local/archives.json" \
		--kv-file "readme=./Server/InigoStatic/Local/readme.json" \
		--kv-file "accounts=./Server/InigoStatic/Local/accounts.json" \
		--kv-file "sessions=./Server/InigoStatic/Local/sessions.json" \
		build/exec/inigo-server

deploy-init :
	terraform init -upgrade -var-file=${HOME}/InigoStatic.tfvars InigoStatic

deploy : static server
	terraform apply -var-file=${HOME}/InigoStatic.tfvars Server/InigoStatic

test :
	idris2 --find-ipkg Test/Suite.idr --cg node -x suite

clean :
	idris2 --clean Inigo.ipkg
	idris2 --clean idrall/idrall.ipkg

testbin :
	${MAKE} -C golden testbin

golden : testbin
	${MAKE} -C golden only=$(only)

distclean : clean
	$(RM) -r build
