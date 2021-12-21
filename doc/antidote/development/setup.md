# Developer Setup and Environment

Antidote requires Erlang 21 or greater. 
Make sure the command `erl` is executable in your console.

Use the following `Makefile` targets to build and test antidote:

	# compile the project:
	make compile

	# run the unit tests:
	make test

	# run the system tests:
	make systests
	
	# run the release test:
	make reltest

	# run dialyzer to check types:
	make dialyzer
	
	# run linter
	make lint

	# open a shell:
	make shell

	# build a release:
	make rel
	
	

Working on dependencies
-----------

When working on dependencies of Antidote it can be helpful to use them as [Checkout Dependencies](https://www.rebar3.org/docs/dependencies#section-checkout-dependencies):

- Create a folder named `_checkouts` in your `antidote` folder (next to the `_build` folder)
- Clone or symlink the dependency into that folder. The folder name in `_checkouts` must be the name of the dependency in `rebar.config`.
- When running a rebar3 task on Antidote, it will always use the latest version from the dependencies. 
  It will also recompile all other dependencies, which can be avoided [by patching rebar3](https://github.com/erlang/rebar3/issues/2152)
