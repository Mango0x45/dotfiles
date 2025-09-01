TEXTDOMAINDIR=/usr/local/share/locale

all:

extract:
	mkdir -p po/sv
	for x in vlt;                                                               \
	do                                                                          \
		xgettext -L Shell -k_ --from-code=UTF-8 -o "po/$$x.pot" "$$x";          \
		for l in sv_SE;                                                         \
		do                                                                      \
			d="$${l%_*}";                                                       \
			if [ -e "po/$$d/$$x.po" ];                                          \
			then                                                                \
				msgmerge -U "po/$$d/$$x.po" "po/$$x.pot";                       \
			else                                                                \
				msginit -l "$$l.UTF-8" -i "po/$$x.pot" -o "po/$$d/$$x.po";      \
			fi;                                                                 \
		done;                                                                   \
	done

compile:
	mkdir -p "$(TEXTDOMAINDIR)/sv/LC_MESSAGES"
	for l in sv;                                                                    \
	do                                                                              \
		for x in vlt;                                                               \
		do                                                                          \
			msgfmt "po/$$l/$$x.po" -o "$(TEXTDOMAINDIR)/$$l/LC_MESSAGES/$$x.mo";    \
		done;                                                                       \
	done
