SBT ?= sbt

# The list of test cases that are run when running "make check"
CHECK += PassingWordLoopback
CHECK += FailingWordLoopback
CHECK += Encoder8b10b
CHECK += Decoder8b10b
CHECK += Serial8b10bController

.PHONY: all
all: obj/hurricane-serial.jar

obj/hurricane-serial.jar: $(find src -iname "*.scala")
	$(SBT) package
	mkdir -p $(dir $@)
	cp $(shell find target -iname "*.jar") $@

.PHONY: check
check: $(patsubst %,check/%.out,$(CHECK))

check/%.out: $(find src -iname "*.scala")
	rm -rf $@.d
	mkdir -p $(dir $@)
	$(SBT) 'run-main SerialTests.$(patsubst %.out,%,$(notdir $@))Tester --targetDir $@.d --genHarness --compile --test --debug --vcd' |& tee $@

.PHONY: clean
clean:
	rm -rf check
	rm -rf obj
