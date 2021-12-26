################################################################################
## CONFIG ######################################################################
################################################################################
SRC_DIR ?= src
WWW_DIR ?= www
WWW_SCRIPT_DIR ?= $(WWW_DIR)/script

ELM_CC ?= elm make

MAIN_MODULE ?= $(SRC_DIR)/Main.elm

################################################################################
## MAKEFILE MAGIC ##############################################################
################################################################################
SUB_MODULES = $(shell find $(SRC_DIR) -type f | grep "elm$$")

################################################################################
## SANITY CHECKS ###############################################################
################################################################################

################################################################################
## TARGET RULES ################################################################
################################################################################
build: $(WWW_SCRIPT_DIR)/main.js

clean:
	rm -f $(WWW_SCRIPT_DIR)/main.js

reset:
	rm -rf elm-stuff

################################################################################
## INTERNAL RULES ##############################################################
################################################################################
$(WWW_SCRIPT_DIR)/main.js: $(MAIN_MODULE) $(SUB_MODULES)
	$(ELM_CC) $(MAIN_MODULE) --output $@
