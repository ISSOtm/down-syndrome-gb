
RESDIR := $(SRCDIR)/res/title_screen


CLEANTARGETS := $(CLEANTARGETS) $(RESDIR)/title_screen.chr $(RESDIR)/title_screen.chr.pb16

INITTARGETS := $(INITTARGETS) $(RESDIR)/title_screen.chr $(RESDIR)/title_screen.chr.pb16 $(RESDIR)/title_screen.tilemap

$(RESDIR)/%.chr $(RESDIR)/%.tilemap: $(RESDIR)/%.png
	rgbgfx -u -t $(RESDIR)/title_screen.tilemap -o $@ $<
