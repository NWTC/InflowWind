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
REGSFLAGS = -ffree-line-length-none


#Destination for compiled pieces
DEST_DIR := ../_compiled--linux

#Location of source files for the NWTC Library
LIB_DIR := \
   ../../../../../NWTC_Library/branches/BJonkman/source

#	../../../../../NWTC_Library/trunk/source

#Name of the executable
HHExecutable_NAME := HHWind_Test.lin32
FFExecutable_NAME := FFWind_Test.lin32

#Name of the registry file
RegistryDIR    := ..
HHRegistryTXT  := Reg-IfW_HHWind.txt
HHRegistryF90	:= IfW_HHWind_Types.f90
HHRegistryOBJ  := $(HHRegistryF90:.f90=.o)

FFRegistryTXT  := Reg-IfW_FFWind.txt
FFRegistryF90	:= IfW_FFWind_Types.f90
FFRegistryOBJ  := $(FFRegistryF90:.f90=.o)

#Where the doxygen generated documentation is stored
Doxy_DIR = ../../Doxygen--documentation


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
InflowWind_SOURCES   :=          \
   SharedInflowDefs.f90          \
   InflowWind_Module_Types.f90

HH_SOURCES           :=          \
	IfW_HHWind.f90                \
   $(HHRegistryF90)

FF_SOURCES           :=          \
	IfW_FFWind.f90                \
   $(FFRegistryF90)

#Testing sources
IfW_HH_Test_SOURCES  :=             \
   IfW_HHWind_Test.f90
IfW_FF_Test_SOURCES  :=             \
   IfW_FFWind_Test.f90

#Registry program
RegistryProgram   := ../../../../../modularization_registry/registry.exe
RegistryNWTC      := ../../../../../modularization_registry/Registry_NWTC_Library.txt
###########
#### You should not need to change anything beyond this point
#rule to do everything
all:           default
SETUP:         $(DEST_DIR) libcopy
HH:            SETUP $(DEST_DIR)/$(HHExecutable_NAME)
FF:            SETUP $(DEST_DIR)/$(FFExecutable_NAME)
default:       HH FF

LIB_OBJS          = $(LIB_SOURCES:.f90=.o)
InflowWind_OBJS   = $(InflowWind_SOURCES:.f90=.o)
HH_OBJS           = $(HH_SOURCES:.f90=.o)
FF_OBJS           = $(FF_SOURCES:.f90=.o)
IfW_HH_Test_OBJS  = $(IfW_HH_Test_SOURCES:.f90=.o)
IfW_FF_Test_OBJS  = $(IfW_FF_Test_SOURCES:.f90=.o)

vpath %.f90 $(LIB_DIR) $(InflowWind_DIR) $(IfW_Test_DIR)
vpath %.o $(DEST_DIR) $(LIB_DIR)

#general rule for making the files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $(DEST_DIR)/$@ -I$(LIB_DIR) -J $(DEST_DIR)

#specific rule for the HHWind registry file
$(HHRegistryF90):   $(RegistryDIR)/$(HHRegistryTXT)
	$(RegistryProgram) $<;
	mv $@ $(RegistryDIR);

#specific rule for the FFWind registry file
$(FFRegistryF90):   $(RegistryDIR)/$(FFRegistryTXT)
	$(RegistryProgram) $<;
	mv $@ $(RegistryDIR);

#Dependency rules
SysGnuLinux.o:          SingPrec.o
NWTC_IO.o:              SysGnuLinux.o
NWTC_Num.o:             NWTC_IO.o
NWTC_Aero.o:            NWTC_IO.o NWTC_Num.o
ModMesh.o:              SingPrec.o NWTC_IO.o ModMesh_Types.o
NWTC_Library.o:         NWTC_Aero.o ModMesh.o

IfW_HHWind_Types.o:     NWTC_Library.o $(HHRegistryF90)
IfW_HHWind.o:           SharedInflowDefs.o InflowWind_Module_Types.o NWTC_Library.o IfW_HHWind_Types.o
IfW_HHWind_Test.o:      IfW_HHWind.o SharedInflowDefs.o NWTC_Library.o IfW_HHWind_Types.o

IfW_FFWind_Types.o:     NWTC_Library.o $(FFRegistryF90)
IfW_FFWind.o:           SharedInflowDefs.o InflowWind_Module_Types.o NWTC_Library.o IfW_FFWind_Types.o
IfW_FFWind_Test.o:      IfW_FFWind.o SharedInflowDefs.o NWTC_Library.o IfW_FFWind_Types.o

#Make sure the destination directory exists
$(DEST_DIR):
	mkdir -p $(DEST_DIR)

#Copy library mod and .o files over: might be able to make this a rule that checks the dates.
libcopy:
	@cp -a $(LIB_DIR)/*.mod $(DEST_DIR); cp -a $(LIB_DIR)/*.o   $(DEST_DIR);

#For compiling IfW_HHWind_Test
$(DEST_DIR)/$(HHExecutable_NAME): $(LIB_OBJS) $(InflowWind_OBJS) $(HH_OBJS) $(IfW_HH_Test_OBJS) | $(DEST_DIR)
	$(FC) $(FFLAGS) -I $(DEST_DIR) -o $(DEST_DIR)/$(HHExecutable_NAME)         \
		$(foreach src, $(LIB_OBJS),         $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(InflowWind_OBJS),  $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(HH_OBJS),          $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(IfW_HH_Test_OBJS), $(addprefix $(DEST_DIR)/,$(src)));
		cp $(DEST_DIR)/$(HHExecutable_NAME) .

#For compiling IfW_FFWind_Test
$(DEST_DIR)/$(FFExecutable_NAME): $(LIB_OBJS) $(InflowWind_OBJS) $(FF_OBJS) $(IfW_FF_Test_OBJS) | $(DEST_DIR)
	$(FC) $(FFLAGS) -I $(DEST_DIR) -o $(DEST_DIR)/$(FFExecutable_NAME)         \
		$(foreach src, $(LIB_OBJS),         $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(InflowWind_OBJS),  $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(FF_OBJS),          $(addprefix $(DEST_DIR)/,$(src)))   \
		$(foreach src, $(IfW_FF_Test_OBJS), $(addprefix $(DEST_DIR)/,$(src)));
		cp $(DEST_DIR)/$(FFExecutable_NAME) .


#cleanup afterwards
clean:
	rm -f $(DEST_DIR)/*.o $(DEST_DIR)/*.mod $(DEST_DIR)/$(HHExecutable_NAME) $(DEST_DIR)/$(FFExecutable_NAME);
	rmdir $(DEST_DIR);
	rm -f $(RegistryDIR)/$(HHRegistryF90) $(RegistryDIR)/$(FFRegistryF90);

