EXECUTABLES = grmap.x grmean.x grsplit.x grtran.x ibm2ie3.x cray2ie3.x grvar.x
OBJS = ${EXECUTABLES:.x=.o}

SHELL = /bin/sh
CP = cp
RM = rm
CD = cd
CHMOD = chmod
CC = cc
CPP = cpp -C

LIB_DIR = /disk4/shong/GMP/libs/lib
ETC_DIR = /disk4/shong/GMP/libs/etc
F77 = f90
FORT_FLAGS = -r8 -O3 -fpe1 -i4
LOAD_FLAGS = -r8 -O3 -fpe1 -i4
INCLUDE_DIR = -I.. -I../..

LIB = ${LIB_DIR}/w3lib.a ${LIB_DIR}/libncaru.a

execs : ${EXECUTABLES}
	${CP} $? ${ETC_DIR}
	${CD} ${ETC_DIR}; \
	${CHMOD} 755 $?

.SUFFIXES : .x .o .f90 .F90 

.F90.f90 :
	${CPP} -P ${INCLUDE_DIR} $*.F90 >$*.i
	sed '/^ *$$/d' $*.i >$*.f90 ; rm $*.i

.F90.o :
	${CPP} -P ${INCLUDE_DIR} $*.F90 >$*.i
	sed '/^ *$$/d' $*.i >$*.f90 ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f90

.F90.x :
	${CPP} -P ${INCLUDE_DIR} $*.F90 >$*.i
	sed '/^ *$$/d' $*.i >$*.f90 ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f90
	${F77} ${LOAD_FLAGS} -o $@ $*.o ${LIB}

.o.x :
	${F77} ${LOAD_FLAGS} -o $@ $< ${LIB}

.f90.x :
	${F77} ${FORT_FLAGS} -c $<
	${F77} ${LOAD_FLAGS} -o $@ $*.o ${LIB}

.f90.o :
	${F77} ${FORT_FLAGS} -c $<

clean :
	- ${RM} -rf *.f90 *.o *.x core rii_files
	- ${CD} ${ETC_DIR}; \
	${RM} -f ${EXECUTABLES}

# DO NOT DELETE
