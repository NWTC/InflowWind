# 2012.12.11
#This makefile has been tested on Ubuntu 12.10 with gfortran 4.7.2
#It is intended to be used with
#     NWTC Subroutine Library (v1.06.00,      ??-Dec-2012)
#
#Older versions of the library and WT_Perf may not work with this makefile

#compiler to use
FC = gfortran
#compiler flags
FFLAGS = -O3 -fbounds-check -fbacktrace #-m32 -g 

#Destination for compiled pieces
DEST_DIR := compiled--linux

#Location of source files for the NWTC Library
LIB_DIR := \
	$(HOME)/software-development/NWTC_Library--source/source

#Name of the executable
Executable_NAME := InflowWind.linux

#Grab the current revision info and store it
#	-- it would be nice to add one so it corresponds to the next commit
#		-- would require calling a script though -- that becomes platform dependent though.
REVNUM := `svn info |grep Revision: | grep -o [0-9]*`

#Where the doxygen generated documentation is stored
Doxy_DIR = ../Doxygen--documentation

#Change SingPrec.f90 to DoubPrec.f90 for double precision
LIB_SOURCES := \
	SingPrec.f90 \
	SysGnuLinux.f90 \
	NWTC_IO.f90 \
	NWTC_Num.f90 \
	ModMesh.f90 \
	NWTC_Aero.f90 \
	NWTC_Library.f90

#Location of the source files for WT_Perf
InflowWind_DIR      := \
	.

#Location of the fortran driver code
Other_DIR	:= \
	../TestRoutines/Fortran/

#The source files for InflowWind
InflowWind_SOURCES  := \
	SharedInflowDefs.f90 \
   WindFile_Types.f90 \
	CTWind.f90 \
	FDWind.f90 \
	FFWind.f90 \
	HAWCWind.f90 \
	HHWind.f90 \
	UserWind.f90 \
   InflowWind_Subs.f90 \
	InflowWind_Module.f90 \
   InflowWind_Driver.f90

#   InflowWind_Test.f90





vpath %.f90 $(LIB_DIR) $(InflowWind_DIR) $(InflowWind_Driver_DIR)
vpath %.o $(DEST_DIR)

###########
#### You should not need to change anything beyond this point
LIB_OBJS = $(LIB_SOURCES:.f90=.o)
InflowWind_OBJS = $(InflowWind_SOURCES:.f90=.o)

#rule to do everything
all:     default
InflowWind:    clean default
default: $(DEST_DIR) $(DEST_DIR)/$(Executable_NAME)

#general rule for making the files
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $(DEST_DIR)/$@ -J $(DEST_DIR)


#Dependency rules
SysGnuLinux.o:    SingPrec.o
NWTC_IO.o:        SysGnuLinux.o
NWTC_Num.o:       NWTC_IO.o
NWTC_Aero.o:      NWTC_IO.o NWTC_Num.o
NWTC_Library.o:   NWTC_Aero.o ModMesh.o
InflowWind_Module.o:    SharedInflowDefs.o InflowWind_Subs.o
InflowWind_Subs.o:      SharedInflowDefs.o
InflowWind_Test.o:      InflowWind_Subs.o

#Make sure the destination directory exists
$(DEST_DIR):
	mkdir -p $(DEST_DIR)


#For compiling InflowWind
$(DEST_DIR)/$(Executable_NAME): $(LIB_OBJS) $(InflowWind_OBJS) $(InflowWind_Driver_DIR)$(InflowWind_Driver) | $(DEST_DIR)
	$(FC) $(FFLAGS) -I $(DEST_DIR) -o $(DEST_DIR)/$(Executable_NAME) \
		$(foreach src, $(LIB_OBJS), $(addprefix $(DEST_DIR)/,$(src))) \
		$(foreach src, $(InflowWind_OBJS), $(addprefix $(DEST_DIR)/,$(src)));
		cp $(DEST_DIR)/$(Executable_NAME) .


#cleanup afterwards
clean:
	rm -f $(DEST_DIR)/*.o $(DEST_DIR)/*.mod

#doxygen bits
document: all
	rm $(Doxy_DIR)/tempassembled.f90;
	cat $(foreach src, $(InflowWind_SOURCES), $(addprefix $(InflowWind_DIR)/,$(src))) >> $(Doxy_DIR)/tempassembled.f90;
	$(MAKE) -C $(Doxy_DIR);

#	cat $(InflowWind_Driver_DIR)$(InflowWind_Driver) >> $(Doxy_DIR)/tempassembled.f90;
   #This line is for putting all the NWTC_Lib bits in the documentation;
   #	cat $(foreach src, $(LIB_SOURCES), $(addprefix $(LIB_DIR)/,$(src))) >> $(Doxy_DIR)/tempassembled.f90;
