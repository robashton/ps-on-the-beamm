.PHONY: all clean repl

default: output

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)

beams: $(OUTPUT) legacy_src/*.erl
	mkdir -p beams
	erlc -o beams $(OUTPUT)/*/*.erl legacy_src/*.erl 
	touch beams

output: $(PS_SOURCEFILES) $(PS_ERL_FFI) .psc-package
	psc-package sources | xargs purs compile '$(PS_SRC)/**/*.purs'
	@touch $(OUTPUT)

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)
	rm -rf beams
	rm -rf .psc-package

