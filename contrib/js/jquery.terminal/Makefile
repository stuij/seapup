VERSION=0.4.21
JSC=java -jar bin/closure.bin/compiler.jar --js
SED=sed
CP=cp

ALL: js/jquery.terminal-$(VERSION).js js/jquery.terminal-$(VERSION).min.js js/jquery.terminal-min.js README www/Makefile

js/jquery.terminal-$(VERSION).js: js/jquery.terminal-src.js
	$(SED) -e "s/{{VER}}/$(VERSION)/g" -e "s/{{DATE}}/`date -uR`/g" js/jquery.terminal-src.js > js/jquery.terminal-$(VERSION).js

js/jquery.terminal-$(VERSION).min.js: js/jquery.terminal-$(VERSION).js
	$(JSC) js/jquery.terminal-$(VERSION).js > js/jquery.terminal-$(VERSION).min.js

js/jquery.terminal-min.js: js/jquery.terminal-$(VERSION).min.js
	$(CP) js/jquery.terminal-$(VERSION).min.js js/jquery.terminal-min.js	

README: README.in .$(VERSION)
	$(SED) -e "s/{{VER}}/$(VERSION)/g" < README.in > README

.$(VERSION):
	touch .$(VERSION)

www/Makefile: Makefile
	$(SED) -e "s/{{VERSION}}/$(VERSION)/g" www/Makefile.in > www/Makefile
