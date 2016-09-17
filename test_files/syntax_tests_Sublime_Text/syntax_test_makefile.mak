# SYNTAX TEST "Packages/Makefile/Makefile.sublime-syntax"

# Line comment
# <- comment

ifeq ($(shell test -r $(MAKEFILE_PATH)Makefile.Defs; echo $$?), 0)
# <- keyword.control.makefile
#       ^ support.function.builtin.makefile
#                                                  ^ keyword.operator.makefile
#                                                           ^ support.variable.makefile
	include $(MAKEFILE_PATH)Makefile.Defs
	# <- keyword.control.makefile
endif
# <- keyword.control.makefile

-include $(MAKEFILE_PATH)Makefile.Defs
# <- keyword.control.makefile

sinclude $(MAKEFILE_PATH)Makefile.Defs
# <- keyword.control.makefile

CC=g++
# <- variable.other.makefile
# ^ keyword.operator.assignment.makefile
#  ^ string.unquoted.makefile
SOURCES=main.cpp
SOURCES+=hello.cpp
# <- variable.other.makefile
#      ^ keyword.operator.assignment.makefile
#        ^ string.unquoted.makefile

EXECUTABLE=hello

OBJECTS=$(SOURCES:.cpp=.o)
#         ^ string.unquoted.makefile variable.source.makefile

all: $(SOURCES) hello.o
# <- entity.name.function.makefile
#    ^ punctuation.definition.variable.begin.makefile
#      ^ variable.source.makefile

$(EXECUTABLE): $(OBJECTS)
# <- punctuation.definition.variable.begin.makefile
# ^ variable.source.makefile
#              ^ punctuation.definition.variable.begin.makefile
#                ^ variable.source.makefile
	@$(CC) $(LDFLAGS) $(OBJECTS) -o $@
	#   <- support.variable.makefile
	#                               ^ support.variable.makefile

.PHONY: help
# <- constant.other.target.makefile

help::
	@echo 'Excutable is $$(EXECUTABLE)'
	# <- support.variable.makefile
	 # <- support.function.makefile
	#     ^ string.source.makefile
	#                   ^ punctuation.definition.variable.begin.makefile
	#                      ^ string.source.makefile variable.source.makefile
