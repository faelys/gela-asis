##############################################################################
##                           G E L A   A S I S                              ##
##       ASIS implementation for Gela project, a portable Ada compiler      ##
##                        http://gela.ada-ru.org/                           ##
##                     # # # # # # # # # # # # # # #                        ##
##############################################################################
## Makefile has following targets                                           ##
##   all (default) - build gela asis library                                ##
##   install       - build and install gela asis library into ${PREFIX}     ##
##   generate      - generate all Ada files to build gela asis library      ##
##############################################################################

# Configuration variables:                 #################################
# rpm specific
DESTDIR ?= 
# Library version
VERSION ?= .0.3.2
# Place to install
PREFIX           ?= ${HOME}/gela-asis
# libraries directory
LIBDIR 		 ?= ${PREFIX}/lib
# libraries kind (static or relocatable)
LIBKIND		 ?= static
#LIBKIND	 ?= relocatable
# Place to write files during building:
BUILD            ?= $(abspath .build)
# gnatmake flags:
FLAGS            ?= -j0 -g -m -O2
# Where install standard Ada library files
SPECDIR          ?= ${PREFIX}/include/gela-asis/spec/
# Where install project file
GPRDIR           ?= ${LIBDIR}/gnat/
# Project file template
GPR ?= gela_asis.gpr.in
# If you like to use your own ayacc/afles uncomment next three lines:
# BIN_AYACC   ?= /path/to/where/it/is/ayacc
# EXTRA_BUILD ?=
# End of configuration variables.          #################################

# Shortcuts

# Where install puts source files
INC ?= ${PREFIX}/include/gela-asis
# Where install puts library and .ali files
LIB ?= ${LIBDIR}/gela-asis

P            := ${BUILD}/parser

WORK_DIRS    := ${BUILD} ${BUILD}/ada ${BUILD}/xml2ayacc ${P} \
	        ${BUILD}/asis ${BUILD}/uaflex ${BUILD}/ayacc ${BUILD}/obj \
	        ${BUILD}/gela_lib ${BUILD}/lib

BUFFER_SRC   := ${BUILD}/ada/gela-source_buffers-current.ads
XML_TO_Y     := ${BUILD}/xml2ayacc/xml_to_y
XML_TO_Y_SRC := tools/xml2ayacc/*.ad[sb]
UAFLEX_BIN   := ${BUILD}/uaflex/uaflex
UAFLEX_SRC   := tools/uaflex/*.ad[sb]

A            := source

LIBGELA_SRC  := ${A}/libgela/*.ad[sb]
ENCODING_SRC := tools/xml2ayacc/encoding/*.ad[sb] \
 tools/xml2ayacc/encoding/auto/*.ad[sb]
XML_IO_SRC   := tools/xml2ayacc/xml_io/*.ad[sb]
XASIS_SRC    := ${A}/asis/xasis/*.ad[sb]
ASIS_SRC     := ${A}/asis/*.ad[sb]

LIB_STAMP    := ${BUILD}/${LIBKIND}.timestamp
ASIS_STAMP   := ${BUILD}/asis.timestamp
LEXER_STAMP  := ${BUILD}/lexer.timestamp
PARSER_STAMP := ${BUILD}/parser.timestamp

PL           := ${BUILD}/ada/asis-gela-scanner_tables
LEXER_SRC    := ${PL}.adb ${PL}.ads

PP           := ${BUILD}/ada/asis-gela-parser
PARSER_SRC   := ${PP}.ads ${PP}.adb ${PP}-goto_table.ads \
                ${PP}-shift_reduce.ads ${PP}-tokens.ads

XML   := source/asis/model
XSL   := source/asis/xslt
UAFLEX := ${XML}/uaflex
AYACC := ${XML}/ayacc
ASIS  := source/asis
SPEC := source/asis/spec

BIN_INSTALL  ?= install
BIN_XSLTPROC ?= xsltproc
BIN_SED      ?= sed
BIN_ECHO     ?= echo
BIN_CAT      ?= cat
BIN_CP       ?= cp
BIN_GNATCHOP ?= gnatchop
BIN_GNATMAKE ?= gnatmake
BIN_GCC      ?= gcc
BIN_AYACC    ?= ${BUILD}/ayacc/ayacc

EXTRA_BUILD  ?= ${BIN_AYACC}

GNATMAKE := ${BIN_GNATMAKE} ${FLAGS} -XGELA_BUILD=${BUILD} \
  -XLIBRARY_TYPE=${LIBKIND} -XVERSION=${VERSION}

all: ${LIB_STAMP}

generate: make_dirs ${ASIS_STAMP} ${LEXER_STAMP} ${PARSER_STAMP} ${BUFFER_SRC}

make_dirs: ${WORK_DIRS}

${WORK_DIRS}:
	mkdir -p $@

${BUFFER_SRC}:
	${BIN_ECHO} with Gela.Source_Buffers.Portable\; \
        > ${BUFFER_SRC}
	${BIN_ECHO} package Gela.Source_Buffers.Current renames \
                Gela.Source_Buffers.Portable\; \
	>> ${BUFFER_SRC}

${XML_TO_Y}: ${XML_TO_Y_SRC} ${LIBGELA_SRC} ${ENCODING_SRC} ${XML_IO_SRC}
	${GNATMAKE} -P gnat/gela_xml2y.gpr -o ${XML_TO_Y} xml_to_y-driver.adb

${UAFLEX_BIN}: ${UAFLEX_SRC}
	${GNATMAKE} -P gnat/gela_uaflex.gpr -o ${UAFLEX_BIN} uaflex-driver.adb

${LEXER_STAMP}: ${UAFLEX_BIN} ${ASIS}/ada.uaflex
	${UAFLEX_BIN} ${ASIS}/ada.uaflex Asis.Gela.Scanner_Tables \
	${BUILD}/ada
	$(BIN_ECHO) "" > ${LEXER_STAMP}

${PARSER_STAMP}: ${XML_TO_Y} \
               ${XSL}/fixed.xsl      ${XML}/*.xml                   \
               ${XSL}/deep.sed       ${ASIS}/asis-gela-parser.adt   \
               ${EXTRA_BUILD}                                       \

	${BIN_ECHO} > ${P}/parser.y2
	$(BIN_XSLTPROC) -o ${P}/fixed.xml ${XSL}/fixed.xsl ${XML}/syntax.xml
	${XML_TO_Y} ${XML}/asis_hier.xml ${XML}/tokens.xml ${P}/fixed.xml \
           ${XML}/code.xml ${P}/parser.y2
	${BIN_ECHO} %token New_Line_Token      > ${P}/parser.y1
	${BIN_ECHO} %token Separator_Token    >> ${P}/parser.y1
	${BIN_ECHO} %token Comment_Token      >> ${P}/parser.y1
	$(BIN_CAT) ${P}/parser.y? ${ASIS}/asis-gela-parser.adt >> ${P}/parser.y
	$(BIN_AYACC) ${P}/parser.y
	$(BIN_SED) -f ${XSL}/deep.sed ${P}/parser*.ad[sb] > ${P}/parser.all
	$(BIN_GNATCHOP) -w ${P}/parser.all ${BUILD}/ada
	$(BIN_ECHO) "" > ${PARSER_STAMP}


${ASIS_STAMP}: ${XML}/asis_hier.xml ${XSL}/asis-gela-elements.xsl \
             ${ASIS}/asis.spec    ${XSL}/asis-ads.xsl           \
             ${ASIS}/asis.body    ${XSL}/asis-adb.xsl           \

	$(BIN_CP)       ${ASIS}/asis.spec ${BUILD}/ada/asis.ads
	$(BIN_XSLTPROC) ${XSL}/asis-ads.xsl ${XML}/asis_hier.xml >> \
	   ${BUILD}/ada/asis.ads
	$(BIN_CP)       ${ASIS}/asis.body ${BUILD}/ada/asis.adb
	$(BIN_XSLTPROC) ${XSL}/asis-adb.xsl ${XML}/asis_hier.xml >> \
	   ${BUILD}/ada/asis.adb
	$(BIN_XSLTPROC) -o ${BUILD}/asis/elems.all \
	   ${XSL}/asis-gela-elements.xsl \
           ${XML}/asis_hier.xml
	$(BIN_SED) -i -e '/^#/d' ${BUILD}/asis/elems.all
	$(BIN_GNATCHOP) -w ${BUILD}/asis/elems.all ${BUILD}/ada
	$(BIN_ECHO) "" > ${ASIS_STAMP}

${LIB_STAMP}: ${WORK_DIRS}
	${GNATMAKE} -P gnat/gela_asis.gpr
	$(BIN_ECHO) "" > ${LIB_STAMP}

install: ${ASIS_STAMP}
	${BIN_INSTALL} -d ${DESTDIR}/${INC}
	${BIN_INSTALL} -d ${DESTDIR}/${LIB}
	${BIN_INSTALL} -d ${DESTDIR}/${SPECDIR}
	${BIN_INSTALL} -d ${DESTDIR}/${GPRDIR}
	${BIN_INSTALL} -m 644 ${BUILD}/ada/*.ad[sb]      ${DESTDIR}/${INC}
	${BIN_INSTALL} -m 644 ${A}/libgela/*.ad[sb]      ${DESTDIR}/${INC}
	${BIN_INSTALL} -m 644 ${A}/asis/xasis/*.ad[sb]   ${DESTDIR}/${INC}
	${BIN_INSTALL} -m 644 ${A}/asis/*.ad[sb]         ${DESTDIR}/${INC}
	${BIN_INSTALL} -m 444 ${BUILD}/lib/*.ali         ${DESTDIR}/${LIB}
	${BIN_INSTALL} -m 644 ${BUILD}/lib/*gela-asis*   ${DESTDIR}/${LIB}
	${BIN_INSTALL} -m 644 ${SPEC}/*.ads              ${DESTDIR}/${SPECDIR}
	${BIN_INSTALL} -m 644 ${SPEC}/annex*/*.ads       ${DESTDIR}/${SPECDIR}
	${BIN_INSTALL} -m 644 ${SPEC}/obsolescent*/*.ads ${DESTDIR}/${SPECDIR}
	${BIN_SED} -e "s#@Library_Kind@#${LIBKIND}#g" \
		-e "s#@Source_Dirs@#${INC}#g" \
		-e "s#@Library_Dir@#${LIB}#g" \
		gnat/${GPR} >  ${DESTDIR}/${GPRDIR}/gela_asis.gpr

	@${BIN_ECHO} Please add next variable to your environmet:
	@${BIN_ECHO} GELA_INCLUDE_PATH=${SPECDIR}

${BIN_AYACC}: contrib/ayacc/src/*.ad[sb]
	${GNATMAKE} -P gnat/contrib_ayacc.gpr -o ${BIN_AYACC}

clean:
	rm -rf ${WORK_DIRS}

AV_SRC  := examples/asis_view_gtk
AV_OUT  := ${BUILD}/asis_view
AV_HIER := ${AV_OUT}/hier.xml

asis_view: ${AV_OUT}
	$(BIN_XSLTPROC) -o ${AV_HIER} ${AV_SRC}/rm_abs.xsl ${XML}/asis_hier.xml
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_element_frames.ads \
	    ${AV_SRC}/frames.xsl ${AV_HIER}
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_elements-create_frames.adb \
	    ${AV_SRC}/create.xsl ${AV_HIER}
	$(BIN_XSLTPROC) -o ${AV_OUT}/gtk_asis_elements-show.adb \
	    ${AV_SRC}/show.xsl ${AV_HIER}
	${BIN_GNATMAKE} -P ${AV_SRC}/asis_view.gpr

${AV_OUT}:
	mkdir -p ${AV_OUT}
