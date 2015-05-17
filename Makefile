SBT ?= sbt

# The list of test cases that are run when running "make check"
CHECK += check/PassingWordLoopbackTester.out.gz
CHECK += check/FailingWordLoopbackTester.out.gz
CHECK += check/Encoder8b10bTester.out.gz
CHECK += check/Decoder8b10bTester.out.gz
CHECK += check/Serial8b10bControllerTester.out.gz

# Obtains the package version from SBT
VERSION = $(shell cat build.sbt  |  grep "^version :=" | tail -n1 | cut -d' ' -f3 | sed 's/"//g')
SCALA_VERSION = $(shell cat build.sbt  |  grep "^scalaVersion :=" | tail -n1 | cut -d' ' -f3 | sed 's/"//g' | cut -d. -f1-2)

# The package name is actually just the name of the directory this
# project is stored in.
PACKAGE_NAME = $(shell pwd | xargs basename)

# The default target is "all", so "make" is the same as "make all".
.PHONY: all
all:

# The "clean" rule removes everything that could have been generated
# by this build (distclean is the same).
.PHONY: clean
clean::
	rm -rf obj project check target lib

.PHONY: distclean
distclean::
	$(MAKE) clean

# Runs all the tests, and the proceeds to look through the output to
# make sure they passed.
.PHONY: check
check:: $(CHECK)
	@for f in $^; do gzip -dc $$f | tail -n1 | grep -q 'success' && echo "PASS $$f" || true; done
	@for f in $^; do gzip -dc $$f | tail -n1 | grep -q 'success' || echo "FAIL $$f" || true; done
	@for f in $^; do gzip -dc $$f | tail -n1 | grep -q 'success'; done

# The macheniry below actually runs the tests -- this rule runs the
# Chisel tester code to generate an output log, and the one below
# that's somewhat paired with it runs Chisel to generate an object.
check/%.out.gz: obj/check/%/stamp
	mkdir -p $(dir $@)
	rm -f $@
	$(dir $<)/generate --test --targetDir $(dir $<)/targetDir |& gzip > $@ || true

obj/check/%/stamp: obj/check/%/generate
	rm -rf $(dir $@)/targetDir
	mkdir -p $(dir $@)/targetDir
	chisel-hdrtar | tar -xC $(dir $@)/targetDir
	$^ --targetDir $(dir $@)/targetDir --genHarness --compile
	touch $@

obj/check/%/generate: lib/lib$(PACKAGE_NAME).jar
	mkdir -p $(dir $@)
	pscalald --main SerialTests.$* `pkg-config chisel --libs` $^ -o $@

# Generates a library version of this project -- this isn't super
# useful in and of itself, but it does allow me to avoid having sbt
# build all my sources twice, and since sbt does all sorts of parallel
# building itself this is nice.
all: lib/lib$(PACKAGE_NAME).jar
lib/lib$(PACKAGE_NAME).jar: target/scala-$(SCALA_VERSION)/$(PACKAGE_NAME)_$(SCALA_VERSION)-$(VERSION).jar
	mkdir -p $(dir $@)
	cp -f $^ $@

target/scala-$(SCALA_VERSION)/$(PACKAGE_NAME)_$(SCALA_VERSION)-$(VERSION).jar: src/main/scala/*.scala
	find src/main/scala -iname "*.scala" | xargs pscalac `pkg-config chisel --libs` -o $@
	test -f $@
