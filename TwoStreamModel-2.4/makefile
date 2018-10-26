# FILE:
#    makefile
#
# PURPOSE:
#    instructions to the make software to build the 2stream and Simple2stream packages.
#    Please refer to the INSTALL file for further information.
#
# The USAGE file will be created at the same time. This file can be obtained by using the -h flag of
# the 2stream software.
# 

MV = /bin/mv
SHELL = /bin/sh

#######LINUX gcc 
CC       = /usr/bin/gcc40
CCFLAGS  = 
C99FLAG  = -std=c99 
OPTIMIZE = -g 
LDFLAGS  = -lm

FC       = gfortran -std=f95 -ffree-form 
FCFLAGS   = -g -Wall
FLINKFLAG = -Wl,-rpath /usr/local/gcc40/lib

#######SUN Workshop cc 
#CC       = /opt/SUNWspro/bin/cc
#CCFLAGS  = 
#C99FLAG  =
#OPTIMIZE = -g
#LDFLAGS  = -lm
#
#FC       = f90 
#FCFLAGS   = -g -free -M./rad2sf90
#FLINKFLAG = 

#######IBM Aix xlc
#CC       = xlc
#CCFLAGS  = 
#C99FLAG  =
#OPTIMIZE = -g
#LDFLAGS  = -lm

#FC        = /usr/bin/xlf90
#FCFLAGS   = -g -qfree=f90

CFLAGS = $(CCFLAGS) $(OPTIMIZE) -I./rad2s
FFLAGS = $(FCFLAGS) -I./rad2sf90 

MODELS_C_SRC = rad2s/gammaproc.c rad2s/black_canopy.c rad2s/black_background.c
MODELS_C_AUX_SRC = srcC/2streamsRTmodels.c srcC/gamma.c srcC/interf.c srcC/usageMessage.c
MODELS_C_OBJ = $(MODELS_C_SRC:.c=.o)
MODELS_C_AUX_OBJ = $(MODELS_C_AUX_SRC:.c=.o)
	
MODELS_F90_SRC = rad2sf90/gammaproc.f rad2sf90/black_canopy.f rad2sf90/black_background.f
MODELS_F90_MOD = $(MODELS_F90_SRC:.f=.mod)
MODELS_F90_OBJ = $(MODELS_F90_SRC:.f=.o)


all : Cexec 

Cexec : bin/2stream bin/Simple2stream readme

Fexec : bin/Simple2streamF

# compiling C programs 
bin/2stream : srcC/2streams.o $(MODELS_C_AUX_OBJ) $(MODELS_C_OBJ) 
	$(CC) $(CFLAGS) -o bin/2stream srcC/2streams.o $(MODELS_C_OBJ) $(MODELS_C_AUX_OBJ) $(LDFLAGS)
	@echo "-->Compilation/Linking of 2stream done<--"
	@echo "You can readily execute [:prompt:] ./bin/2stream"

bin/Simple2stream : srcC/Simple2stream.o $(MODELS_C_OBJ) 
	$(CC) $(CFLAGS) -o bin/Simple2stream srcC/Simple2stream.o $(MODELS_C_OBJ) $(LDFLAGS)
	@echo "-->Compilation/Linking of Simple2stream done<--"
	@echo "You can readily execute [:prompt:] ./bin/Simple2stream"

srcC/2streams.o         : srcC/2streams.c srcC/common.h rad2s/commonrad.h srcC/interf.h
srcC/Simple2stream.o    : srcC/Simple2stream.c srcC/common.h rad2s/commonrad.h rad2s/black_canopy.h \
	rad2s/black_background.h rad2s/gammaproc.h
srcC/2streamsRTmodels.o : srcC/2streamsRTmodels.c srcC/common.h rad2s/commonrad.h srcC/interf.h srcC/gamma.h \
	srcC/2streamsRTmodels.h rad2s/black_canopy.h rad2s/black_background.h
srcC/gamma.o            : srcC/gamma.c srcC/common.h srcC/interf.h rad2s/gammaproc.h srcC/gamma.h rad2s/commonrad.h
rad2s/black_canopy.o     : rad2s/black_canopy.c rad2s/black_canopy.h
rad2s/black_background.o : rad2s/black_background.c rad2s/black_background.h
rad2s/gammaproc.o        : rad2s/gammaproc.c rad2s/gammaproc.h 
srcC/usageMessage.o     : srcC/usageMessage.c srcC/usageMessage.h srcC/common.h

srcC/interf.o           : srcC/interf.c srcC/common.h srcC/interf.h srcC/gamma.h srcC/2streamsRTmodels.h srcC/usageMessage.h
	$(CC) $(CFLAGS) $(C99FLAG) -c -o srcC/interf.o srcC/interf.c

%.o : %.c
	$(CC) $(CFLAGS) $*.c -c -o $@

#Compiling F90 procedures
bin/Simple2streamF : srcF/Simple2streamF.o 
	$(FC) $(FFLAGS) $(FLINKFLAG) srcF/Simple2streamF.o $(MODELS_F90_OBJ) -o bin/Simple2streamF -lm
	@echo "-->Compilation/Linking of Simple2streamF done<--"
	@echo "You can readily execute [:prompt:] ./bin/Simple2streamF"

srcF/Simple2streamF.o : srcF/Simple2streamF.f $(MODELS_F90_MOD)
	$(FC) $(FFLAGS) -c srcF/Simple2streamF.f -o srcF/Simple2streamF.o

rad2sf90/black_background.mod : rad2sf90/black_background.f
	$(FC) $(FFLAGS) -c rad2sf90/black_background.f -o rad2sf90/black_background.o 
	@$(MV) ./black_background.mod rad2sf90

rad2sf90/black_canopy.mod : rad2sf90/black_canopy.f
	$(FC) $(FFLAGS) -c rad2sf90/black_canopy.f -o rad2sf90/black_canopy.o 
	@$(MV) ./black_canopy.mod rad2sf90

rad2sf90/gammaproc.mod : rad2sf90/gammaproc.f
	$(FC) $(FFLAGS) -c rad2sf90/gammaproc.f -o rad2sf90/gammaproc.o
	@$(MV) ./gammaproc.mod rad2sf90

#Other rules
readme : bin/2stream
	@bin/2stream -h 2> ./USAGE
	
clean :
	rm -f *.o *.mod srcC/*.o srcF/*.o srcF/*.mod rad2s/*.o rad2sf90/*.o rad2sf90/*.mod

realclean : clean
	rm -f bin/2stream bin/Simple2stream bin/Simple2streamF ./USAGE
	
