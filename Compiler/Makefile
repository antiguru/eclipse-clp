#
# ECLiPSe III Compiler
#
# $Id: Makefile,v 1.2 2007/02/22 01:31:56 jschimpf Exp $
#
# BEGIN LICENSE BLOCK
# Version: CMPL 1.1
#
# The contents of this file are subject to the Cisco-style Mozilla Public
# License Version 1.1 (the "License"); you may not use this file except
# in compliance with the License.  You may obtain a copy of the License
# at www.eclipse-clp.org/license.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License. 
# 
# The Original Code is  The ECLiPSe Constraint Logic Programming System. 
# The Initial Developer of the Original Code is  Cisco Systems, Inc. 
# Portions created by the Initial Developer are
# Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
# 
# Contributor(s): Joachim Schimpf.
# 
# END LICENSE BLOCK

ECLIPSEDIR = /vol/Eclipse/prerelease
PREFIX = $(ECLIPSEDIR)
AUX_ECLIPSE = $(ECLIPSEDIR)/bin/$(ARCH)/eclipse
TESTDIR = ~/Tests/Main

MODULE_FILES = \
	compiler_analysis.ecl \
	compiler_codegen.ecl \
	compiler_common.ecl \
	compiler_indexing.ecl \
	compiler_normalise.ecl \
	compiler_peephole.ecl \
	compiler_regassign.ecl \
	compiler_top.ecl \
	compiler_varclass.ecl

INCLUDE_FILES = \
	compiler_compound.ecl

OTHER_FILES = \
	compiler_test.ecl

PRIVATE_ECI = $(MODULE_FILES:%.ecl=%.eci)

PUBLIC_ECI =
PUBLIC_ECO = $(MODULE_FILES:%.ecl=$(PREFIX)/lib/%.eco)


default:
	@echo "You can make the following:"
	@echo "  make doc		Generate documentation in doc/"
	@echo "  make test		Run unit tests"
	@echo "  make cover		Run unit tests and produce output in coverage/"
	@echo "  make test_chat	Test Chat application"
	@echo "  make pretty		Pretty-print sources in pretty/"



PERM = a+r,u+w,go-w
DIRPERM = 2755

.SUFFIXES:	$(SUFFIXES) .ecl .eci .eco

%/.stamp:
	mkdir -p $(@D)
	@/bin/chmod $(DIRPERM) $(@D)
	@touch $@; /bin/chmod $(PERM) $@


#----------------------------------------------------------------------
# Cleanup
#----------------------------------------------------------------------

clean:
	rm -rf *.eci *.eco *.res doc pretty coverage


#----------------------------------------------------------------------
# Test
#----------------------------------------------------------------------

# Run unit tests and compare with reference output

test:
	@$(AUX_ECLIPSE) -b compiler_test.ecl -e ftest
	@if diff test.ref test.res ; then echo Compiler unit test ok ; \
	else echo Compiler unit test failed ; fi


# Test Chat application

test_chat:
	@$(AUX_ECLIPSE) -e \
		'ensure_loaded(compiler_top),\
		 set_flag(gc_interval,10000),\
		 cd("$(TESTDIR)/Chatq"),\
		 [chat],\
		 open("chat.res",write,output),\
		 @(call(hi(demo)),chat),\
		 close(output)'
	@if diff $(TESTDIR)/Chatq/chat.ref $(TESTDIR)/Chatq/chat.res ; then echo Chat test ok ; \
	else echo Chat test failed ; fi



# Run unit tests and generate coverage output in coverage/ subdirectory

cover:
	@$(AUX_ECLIPSE) -b compiler_test.ecl -e ctest


#----------------------------------------------------------------------
# Generate internal documentation
#----------------------------------------------------------------------

%.eci:	%.ecl
	$(AUX_ECLIPSE) -e 'lib(document),icompile("$(*F)","$(@D)")'

doc:	doc/index.html

doc/index.html:	$(PRIVATE_ECI)
	$(AUX_ECLIPSE) -e 'lib(document),ecis_to_htmls(.,doc,"","ECLiPSe III Compiler")'


# Pretty-print the sources

pretty:	pretty/index.html

pretty/index.html: $(MODULE_FILES)
	-@$(AUX_ECLIPSE) -e \
	    "lib(pretty_printer),argv(all,[_|F]),pretty_print(F,[link_back:\"../doc/index.html\"])" \
	    -- $(MODULE_FILES)


#----------------------------------------------------------------------
# Installation
#----------------------------------------------------------------------

install:	$(PUBLIC_ECI) $(PUBLIC_ECO)

$(PREFIX)/lib/%.eco:	%.ecl $(PREFIX)/lib/.stamp
	$(AUX_ECLIPSE) -e 'lib(fcompile), set_flag(variable_names,off), set_flag(debug_compile,off), fcompile("$(*F)", [outdir:"$(@D)"])'
	-@/bin/chmod $(PERM) $@

$(PREFIX)/lib/%.eci:	%.ecl $(PREFIX)/lib/.stamp
	$(AUX_ECLIPSE) -e 'lib(document),icompile("$(*F)","$(@D)")'
	-@/bin/chmod $(PERM) $@

$(PREFIX)/lib/%.ecl:	%.ecl $(PREFIX)/lib/.stamp
	/bin/cp $< $@
	@/bin/chmod $(PERM) $@


