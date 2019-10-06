.PHONY: all ghci play test
STACK := stack --verbosity warn
TERM := xfce4-terminal --geometry=95x10 \
--hide-menubar --hide-toolbar --hide-scrollbar \
-T 'Space Rats'

play:
	@$(STACK) build
	$(TERM) -e 'stack exec sr-exe'

ghci:
	@$(STACK) build
	@$(STACK) ghci

test:
	@$(STACK) test
