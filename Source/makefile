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

#architecture
   #32 or 64
BITS:=64
   #lin or win
OS:=lin

#compiler flags
FFLAGS = -O3 -fbounds-check -fbacktrace -m$(BITS) #-g

#Destination for compiled pieces
DEST_DIR := _compiled--linux

#Location of source files for the NWTC Library
LIB_DIR := \
   ../../../../NWTC_Library/trunk/source

#	../../../../NWTC_Library/trunk/source

#Name of the executable
Executable_ROOT := InflowWind_
Executable_NAME := $(Executable_ROOT)$(OS)$(BITS)

#Where the doxygen generated documentation is stored
Doxy_DIR = ../Doxygen--documentation

#Name of the registry file
RegistryDIR    := .

HHRegistryTXT  := Reg-IfW_HHWind.txt
HHRegistryF90	:= IfW_HHWind_Types.f90
HHRegistryOBJ  := $(HHRegistryF90:.f90=.o)

FFRegistryTXT  := Reg-IfW_FFWind.txt
FFRegistryF90	:= IfW_FFWind_Types.f90
FFRegistryOBJ  := $(FFRegistryF90:.f90=.o)

IfWRegistryTXT := Reg-InflowWind.txt
IfWRegistryF90	:= InflowWind_Types.f90
IfWRegistryOBJ := $(IfWRegistryF90:.f90=.o)



##Change SingPrec.f90 to DoubPrec.f90 for double precision
LIB_SOURCES :=                   \
	SingPrec.f90                  \
	SysGnuLinux.f90               \
	NWTC_IO.f90                   \
	NWTC_Num.f90                  \
	ModMesh_Types.f90             \
	ModMesh.f90                   \
	NWTC_Library.f90

#Location of the source files for WT_Perf
InflowWind_DIR      := \
	.

#Location of the fortran driver code
Other_DIR	:= \
	../TestRoutines/Fortran/

#The source files for InflowWind
InflowWind_SOURCES  :=           \
   InflowWind_Types.f90          \
	IfW_HHWind.f90                \
   IfW_HHWind_Types.f90          \
	IfW_FFWind.f90                \
   IfW_FFWind_Types.f90          \
   InflowWind_Subs.f90           \
	InflowWind.f90                \
   InflowWind_Driver_Types.f90   \
   InflowWind_Driver_Subs.f90    \
   InflowWind_Driver.f90

#	CTWind.f90                    \
	FDWind.f90                    \
	FFWind.f90                    \
	HAWCWind.f90                  \
	UserWind.f90                  \


#Registry program
RegistryProgram   := ../../../../modularization_registry/registry.exe
RegistryNWTC      := ../../../../modularization_registry/Registry_NWTC_Library.txt


vpath %.f90 $(LIB_DIR) $(InflowWind_DIR) $(InflowWind_Driver_DIR)
vpath %.o $(DEST_DIR) $(LIB_DIR)

###########
#### You should not need to change anything beyond this point
LIB_OBJS = $(LIB_SOURCES:.f90=.o)
InflowWind_OBJS = $(InflowWind_SOURCES:.f90=.o)

#rule to do everything
all:           default
SETUP:         $(DEST_DIR) libcopy
InflowWind:    clean default
default:       SETUP $(DEST_DIR)/$(Executable_NAME)

#general rule for making the files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $(DEST_DIR)/$@ -I$(LIB_DIR) -J $(DEST_DIR)


#Dependency rules
IfW_HHWind_Types.o:     $(HHRegistryF90)
IfW_FFWind_Types.o:     $(FFRegistryF90)
IfW_HHWind.o:           IfW_HHWind_Types.o
IfW_FFWind.o:           IfW_FFWind_Types.o
InflowWind_Types.o:     IfW_HHWind_Types.o IfW_FFWind_Types.o
InflowWind_Module.o:    InflowWind_Types.o InflowWind_Subs.o IfW_HHWind.o IfW_FFWind.o
InflowWind_Subs.o:      InflowWind_Types.o
InflowWind_Driver.o:    InflowWind_Subs.o InflowWind_Module.o

#specific rule for the HHWind registry file
$(HHRegistryF90):   $(RegistryDIR)/$(HHRegistryTXT)
	$(RegistryProgram) $<;

#specific rule for the FFWind registry file
$(FFRegistryF90):   $(RegistryDIR)/$(FFRegistryTXT)
	$(RegistryProgram) $<;

#specific rule for the IfW registry file
$(IfWRegistryF90):   $(RegistryDIR)/$(IfWRegistryTXT)
	$(RegistryProgram) $<;

IfW_HHWind_Types.o:     NWTC_Library.o $(HHRegistryF90)
IfW_FFWind_Types.o:     NWTC_Library.o $(FFRegistryF90)
IfW_IfWWind_Types.o:    NWTC_Library.o $(IfWRegistryF90)

#Make sure the destination directory exists
$(DEST_DIR):
	mkdir -p $(DEST_DIR)

#Copy library mod and .o files over: might be able to make this a rule that checks the dates.
libcopy:
	@cp -a $(LIB_DIR)/*.mod $(DEST_DIR); cp -a $(LIB_DIR)/*.o   $(DEST_DIR);

#For compiling InflowWind
$(DEST_DIR)/$(Executable_NAME): $(InflowWind_OBJS) $(InflowWind_Driver_DIR)$(InflowWind_Driver) | $(DEST_DIR)
	$(FC) $(FFLAGS) -I $(DEST_DIR) -o $(DEST_DIR)/$(Executable_NAME) \
		$(foreach src, $(LIB_OBJS), $(addprefix $(DEST_DIR)/,$(src))) \
		$(foreach src, $(InflowWind_OBJS), $(addprefix $(DEST_DIR)/,$(src)));
		cp $(DEST_DIR)/$(Executable_NAME) .


#cleanup afterwards
clean:
	rm -f $(DEST_DIR)/*.o $(DEST_DIR)/*.mod $(DEST_DIR)/$(Executable_ROOT)*;
	rm -f $(RegistryDIR)/$(HHRegistryF90) $(RegistryDIR)/$(FFRegistryF90) $(RegistryDIR)/$(IfWRegistryF90);
	rmdir $(DEST_DIR);

