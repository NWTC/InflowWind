# 2012.12.11
#This makefile has been tested on Ubuntu 12.10 with gfortran 4.7.2
#It is intended to be used with
#     NWTC Subroutine Library (v1.06.00,      ??-Dec-2012)
#
#Older versions of the library and InflowWind may not work with this makefile
#
#
#THIS IS BROKEN
#  This makefile is currently a bit broken. After several changes were made in the library,
#  it no longer compiles properly with this makefile without modifying the FFLAGS. The correct
#  way to deal with this is to either:
#
#     1) change the library so that those flags are not needed (CPP directives)
#     2) simply include the library libnwtc.a file (didn't figure out yet)
#
#  Either way, this makefile will need to be fixed. Right now it copies the .mod and .o files
#  over from the library and uses them (the -a keeps the old dates, so it kind of works).
#



#compiler to use
FC = gfortran
#compiler flags
FFLAGS = -O3 -fbounds-check -fbacktrace -m32 #-g 

#Destination for compiled pieces
DEST_DIR := ../_compiled--linux

#Location of source files for the NWTC Library
LIB_DIR := \
   ../../../../../NWTC_Library/branches/BJonkman/source

#	../../../../../NWTC_Library/trunk/source

#Name of the executable
Executable_NAME := Wind_Test.lin32


##Change SingPrec.f90 to DoubPrec.f90 for double precision
LIB_SOURCES :=                   \
	SingPrec.f90                  \
	SysGnuLinux.f90               \
	NWTC_IO.f90                   \
	NWTC_Num.f90                  \
	ModMesh_Types.f90             \
	ModMesh.f90                   \
	NWTC_Aero.f90                 \
	NWTC_Library.f90

#Location of the source files for InflowWind
InflowWind_DIR    := \
	..

IfW_Test_DIR      := \
   .

IfW_Driver_DIR    := $(IfW_Test_DIR)

#The source files for InflowWind
InflowWind_SOURCES  :=           \
	HHWind.f90                    \
   SharedInflowDefs.f90          \
   InflowWind_Module_Types.f90


#Testing sources
IfW_Test_SOURCES  :=             \
   HHWind_Test.f90         

vpath %.f90 $(LIB_DIR) $(InflowWind_DIR) $(IfW_Test_DIR)
vpath %.o $(DEST_DIR) $(LIB_DIR)

###########
#### You should not need to change anything beyond this point
LIB_OBJS          = $(LIB_SOURCES:.f90=.o)
InflowWind_OBJS   = $(InflowWind_SOURCES:.f90=.o)
IfW_Test_OBJS     = $(IfW_Test_SOURCES:.f90=.o)

#rule to do everything
all:           default
InflowWind:    clean default
default:       $(DEST_DIR) libcopy $(DEST_DIR)/$(Executable_NAME)

#general rule for making the files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $(DEST_DIR)/$@ -I$(LIB_DIR) -J $(DEST_DIR)


#Dependency rules
SysGnuLinux.o:          SingPrec.o
NWTC_IO.o:              SysGnuLinux.o
NWTC_Num.o:             NWTC_IO.o
NWTC_Aero.o:            NWTC_IO.o NWTC_Num.o
ModMesh.o:              SingPrec.o NWTC_IO.o ModMesh_Types.o
NWTC_Library.o:         NWTC_Aero.o ModMesh.o

HHWind.o:               SharedInflowDefs.o InflowWind_Module_Types.o
HHWind_Test.o:          HHWind.o SharedInflowDefs.o

#Make sure the destination directory exists
$(DEST_DIR):
	mkdir -p $(DEST_DIR)

#Copy library mod and .o files over: might be able to make this a rule that checks the dates.
libcopy:
	@cp -a $(LIB_DIR)/*.mod $(DEST_DIR); cp -a $(LIB_DIR)/*.o   $(DEST_DIR);

#For compiling InflowWind
$(DEST_DIR)/$(Executable_NAME): $(LIB_OBJS) $(InflowWind_OBJS) $(IfW_Test_OBJS) | $(DEST_DIR)
	$(FC) $(FFLAGS) -I $(DEST_DIR) -o $(DEST_DIR)/$(Executable_NAME)           \
		$(foreach src, $(LIB_OBJS),         $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(InflowWind_OBJS),  $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(IfW_Test_OBJS),    $(addprefix $(DEST_DIR)/,$(src)));
		cp $(DEST_DIR)/$(Executable_NAME) .


#cleanup afterwards
clean:
	rm -f $(DEST_DIR)/*.o $(DEST_DIR)/*.mod;
	rmdir $(DEST_DIR)
