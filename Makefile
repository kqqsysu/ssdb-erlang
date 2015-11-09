.PHONY: clean all ebin

EBIN_DIR := "./ebin"
ERL := erl


all: ebin
	$(ERL) -make
	(cp src/ssdb.app.src $(EBIN_DIR)/ssdb.app)

ebin:
	(mkdir -p $(EBIN_DIR))

clean:
	(rm -rf ./ebin/*)
	(rm -rf *.dump)
