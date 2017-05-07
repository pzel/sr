.PHONY: all ghci play test
STACK := stack --verbosity warn
TERM := xfce4-terminal

play:
	@$(STACK) build
	$(TERM) -e 'stack exec sr-exe'

ghci:
	@$(STACK) build
	@$(STACK) ghci

test:
	@$(STACK) test
