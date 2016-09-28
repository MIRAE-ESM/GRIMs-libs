EXECUTABLES = grmap.x grmean.x grsplit.x grtran.x ibm2ie3.x cray2ie3.x grvar.x
OBJS = ${EXECUTABLES:.x=.o}

SHELL = /bin/sh
CP = cp
RM = rm
CD = cd
CHMOD = chmod
CC = cc
CPP = cpp -C

LIB_DIR = /san_home/users/hsy/yours/libs/lib
ETC_DIR = /san_home/users/hsy/yours/libs/etc
F77 = ftn
FORT_FLAGS = -O2 -sdefault64
LOAD_FLAGS = -O2 -sdefault64
INCLUDE_DIR = -I.. -I../..

LIB = ${LIB_DIR}/w3lib.a ${LIB_DIR}/libncaru.a

execs : ${EXECUTABLES}
	${CP} $? ${ETC_DIR}
	${CD} ${ETC_DIR}; \
	${CHMOD} 755 $?

.SUFFIXES : .x .o .f .F

.F.f :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i

.F.o :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f

.F.x :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f
	${F77} ${LOAD_FLAGS} -o $@ $*.o ${LIB}

.o.x :
	${F77} ${LOAD_FLAGS} -o $@ $< ${LIB}

.f.x :
	${F77} ${FORT_FLAGS} -c $<
	${F77} ${LOAD_FLAGS} -o $@ $*.o ${LIB}

.f.o :
	${F77} ${FORT_FLAGS} -c $<

clean :
	- ${RM} -rf *.f *.o *.x core rii_files
	- ${CD} ${ETC_DIR}; \
	${RM} -f ${EXECUTABLES}

# DO NOT DELETE
