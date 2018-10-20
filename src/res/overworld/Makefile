
RESDIR := $(SRCDIR)/res/overworld


CLEANTARGETS := $(CLEANTARGETS) $(RESDIR)/overworld.chr $(RESDIR)/overworld.chr.pb16
CLEANTARGETS := $(CLEANTARGETS) $(RESDIR)/sprites.chr $(RESDIR)/sprites.chr.pb16

INITTARGETS := $(INITTARGETS) $(RESDIR)/overworld.chr $(RESDIR)/overworld.chr.pb16
INITTARGETS := $(INITTARGETS) $(RESDIR)/sprites.chr $(RESDIR)/sprites.chr.pb16

$(RESDIR)/%.chr: $(RESDIR)/%.png
	rgbgfx -h -o $@ $<
