SBT ?= sbt
CXX = g++
CXXFLAGS =

# The list of test cases that are run when running "make check"
CHECK += PassingWordLoopback
CHECK += FailingWordLoopback
CHECK += Encoder8b10b
CHECK += Decoder8b10b
CHECK += Serial8b10bController
CHECKS = $(shell echo $(CHECK) | wc -w)

EMU_DIR = emulator
CHECK_DIR = check

PACKAGE_NAME  = $(shell pwd | xargs basename)
CHISEL_ARGS   = --compile --backend c --test --genHarness --W0W --compileInitializationUnoptimized --noIoDebug
SCALA_PACKAGE = SerialTests

SCALA_SRCS    = $(wildcard src/main/scala/*.scala)

# The default target is "all", so "make" is the same as "make all".
.PHONY: all
all: check

# The "clean" rule removes everything that could have been generated
# by this build (distclean is the same).
.PHONY: clean
clean:
	rm -rf check project target $(EMU_DIR)

$(EMU_DIR)/%/log: $(SCALA_SRCS)
	mkdir -p $(dir $@)
	-$(SBT) "run-main $(SCALA_PACKAGE).$*Tester $(CHISEL_ARGS) --targetDir $(EMU_DIR)/$*" &> $@

passcount: $(addsuffix /log,$(addprefix $(EMU_DIR)/,$(CHECK)))
	#grep PASSED $^ | wc -l > $@

# jcw: surely there's a better way to do this
.PHONY: check
check: passcount
	if [ `cat $@` == $(CHECKS) ] ; then echo "ALL TESTS PASSED"; else echo "`cat $@ | xargs -IX echo $(CHECKS)-X | bc` TESTS FAILED" ; fi
