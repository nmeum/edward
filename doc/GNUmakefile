# Can be set to an URL for any classless CSS framework.
STYLESHEET ?= https://cdn.jsdelivr.net/gh/kognise/water.css@latest/dist/dark.css

DISCOUNT = discount-theme
DISCOUNT_FLAGS = -c +header,+idanchor

SCMDOC = scmdoc
SCMDOC_FLAGS = -s style.css

SCMDOC_FILES = edward.ed.editor.html \
	edward.ed.cmd.html \
	edward.ed.addr.html \
	edward.cli.html \
	edward.util.html \
	edward.parse.html \
	edward.replace.html \
	edward.buffer.html

all: index.html style.css $(SCMDOC_FILES)
clean:
	$(RM) index.html style.css $(SCMDOC_FILES)
style.css:
	curl -sLo style.css $(STYLESHEET)
index.html: index.md page.theme
	$(DISCOUNT) $(DISCOUNT_FLAGS) -o $@ $<

edward.%.html: %.sld
	$(SCMDOC) $(SCMDOC_FLAGS) $<
edward.ed.%.html: ed/%.sld
	$(SCMDOC) $(SCMDOC_FLAGS) $<

# XXX: This essentially duplicates the dependency tracking of edward.egg.
edward.ed.editor.html: ed/editor.sld ed/editor.scm
edward.ed.cmd.html: ed/cmd.sld ed/cmd.scm
edward.ed.addr.html: ed/addr.sld ed/addr.scm
edward.ed.posix.html: ed/posix.sld ed/posix.scm
edward.cli.html: cli.sld cli.scm
edward.util.html: util.sld util.scm
edward.replace.html: replace.sld replace.scm
edward.parse.html: parse.sld parse/parse.scm parse/repl.scm parse/util.scm
edward.buffer.html: buffer.sld buffer/buffer.scm buffer/stack.scm

deploy: index.html style.css $(SCMDOC_FILES)
	rsync $^ magnesium:/var/www/htdocs/files.8pit.net/edward/doc

VPATH = ../lib
.PHONY: all clean deploy
