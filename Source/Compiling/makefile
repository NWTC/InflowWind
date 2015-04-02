#================================================================================#
# This makefile created by B. Jonkman on 1-Jul-2014,                             #
# adapted from FAST (M. Buhl on 25-Jan-2013).                                    #
# (c) 2014 National Renewable Energy Laboratory                                  #
#                                                                                #
#                                                                                #
#================================================================================#

   # 32-bit or 64-bit?
BITS = 32
#BITS = 64

#OS=lin

   # Location of source files for InflowWind, NWTC Library, and the FAST Registry.
   # You will probably need to change these for your system.



ifeq ($(OS),Windows_NT)
   RegistryProgram   = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/bin/Registry_win32.exe
   NWTC_Lib_DIR      = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source/dependencies/NWTC_Library
   IfW_DIR           = ..
   RegistryDIR       = ../Registry
   Driver_DIR        = ../Driver
   RegistryNWTC      = $(NWTC_Lib_DIR)         # Location of the Registry_NWTC_Library.txt file
else
#   ifeq ($(HOSTNAME), aplatt-18890s)
      SoftwareDir       = /Users/aplatt/software-development/windsvn
      RegistryProgram   = $(SoftwareDir)/FAST/branches/FAST_Registry/registry_Mac.exe
      NWTC_Lib_DIR      = $(SoftwareDir)/NWTC_Library/trunk/source
      IfW_DIR           = $(SoftwareDir)/InflowWind/branches/modularization2/Source
#      RegistryDIR       = $(IfW_DIR)/Registry
		RegistryDIR			= ../Registry
      Driver_DIR        = $(IfW_DIR)/Driver
      RegistryNWTC      = $(NWTC_Lib_DIR)         # Location of the Registry_NWTC_Library.txt file
		RegistryNWTC		= ../../../../../NWTC_Library/trunk/source
		NETLIB_DIR 			:= \
			$(SoftwareDir)/NWTC_Library/branches/NetLib/NWTC_Source/                    	\
			$(SoftwareDir)/NWTC_Library/branches/NetLib/scalapack/scalapack-2.0.2/SRC/    \
			$(SoftwareDir)/NWTC_Library/branches/NetLib/fftpack
			# Mac flag requirement
		LDFLAGS = -framework Accelerate
      FC = gfortran

#   else
#      RegistryProgram   = registry.exe
#      NWTC_Lib_DIR      = $(FAST_DIR)/dependencies/NWTC_Library
#      IfW_DIR           = $(FAST_DIR)/dependencies/InflowWind
#      RegistryDIR       = ../Registry
#      Driver_DIR        = ../Driver
#      RegistryNWTC      = $(NWTC_Lib_DIR)         # Location of the Registry_NWTC_Library.txt file
#   endif
endif


   # Name of compiler to use and flags to use.
#FC      = gfortran

FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -C -DNO_MESHMAPPING \
   -Wall -Wextra -Wconversion -Wpedantic  -Wno-unused-variable -Wno-unused-function -Wno-unused-dummy-argument -Wno-unused-parameter -fno-inline 
   -DFPE_TRAP_ENABLED \
   -ffpe-trap=overflow,invalid,zero ,denormal,inexact,underflow \
   -fdump-fortran-optimized -fdump-fortran-original
#FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -C -DNO_MESHMAPPING
#FFLAGS = -g -O0 -v -da -Q -fbacktrace -ffree-line-length-none -x f95-cpp-input -DNO_MESHMAPPING


	# Mac needs some additional flags prepended for linking to get lapack working, hence the funny notation here.
LDFLAGS := $(LDFLAGS)	-O2 -m$(BITS) -fbacktrace -g -fno-inline 



# some useful gfortran options:
#  -DFPE_TRAP_ENABLED
#  -Wl,--large-address-aware   # not necessary when $(BITS) is 64 (64-bit target)
#  -Wl,--stack=999999999       # not necessary when $(BITS) is 64
#  -Wconversion-extra -Wconversion
#  -fdefault-real-8 -fcheck=bounds,do,mem,pointer -std=f2003 -O0 -v -Wall
# http://gcc.gnu.org/onlinedocs/gfortran/Option-Index.html#Option-Index
#
# makefile:
# --warn-undefined-variables


   # Precision.

# Use "SingPrec" for single precision and "DoubPrec" for double precision. You may also need to change an option switch to make constants DP.
PREC = SingPrec



   # Destination and RootName for executable

OUTPUT_NAME = InflowWind_Driver
DEST_DIR    = ../

   #==========================================================#
   # You should not need to change anything beyond this point #
   #==========================================================#

   # System-specific settings.

ifeq ($(OS),Windows_NT)
      # Windows
   DEL_CMD   = del
   EXE_EXT   = _gwin$(BITS).exe
   INTER_DIR = Obj_win$(BITS)
   MD_CMD    = @mkdir
   OBJ_EXT   = .obj
   PATH_SEP  = \\
   SYS_FILE  = SysGnuWin
else
      # Linux or Mac
   DEL_CMD   = rm -f
   EXE_EXT   = _glin$(BITS)
   INTER_DIR = Obj_lin$(BITS)
   MD_CMD    = @mkdir -p
   OBJ_EXT   = .o
   PATH_SEP  = /
   SYS_FILE  = SysGnuLinux
endif


   # Source files (by module)

LIB_SOURCES =           	\
	$(PREC).f90          	\
	NWTC_Base.f90        	\
	$(SYS_FILE).f90      	\
	NWTC_Library_Types.f90 	\
	NWTC_IO.f90          	\
	NWTC_Num.f90         	\
	ModMesh_Types.f90    	\
	ModMesh.f90          	\
	ModMesh_Mapping.f90  	\
	NWTC_Library.f90

NETLIB_SOURCES :=          \
	NWTC_FFTPACK.f90	   	\
	fftpack4.1.f				\
	NWTC_LAPACK.f90			\
	NWTC_ScaLAPACK.f90		\
	dlasrt2.f				  	\
	slasrt2.f                     

IfW_SOURCES =              \
	IfW_UniformWind_Types.f90  \
	IfW_UniformWind.f90     \
	IfW_HAWCWind_Types.f90  \
	InflowWind_Types.f90    \
	InflowWind_Subs.f90     \
	InflowWind.f90

#	IfW_FFWind_Types.f90    \
	IfW_FFWind.f90          \

#	InflowWind_Input.f90    \


Driver_SOURCES =                 \
	InflowWind_Driver.f90         \
	InflowWind_Driver_Types.f90   \
	InflowWind_Driver_Subs.f90



vpath %.f90 $(NWTC_Lib_DIR) $(IfW_DIR) $(Driver_DIR) $(RegistryDIR) $(NETLIB_DIR)
vpath %.f 	$(NETLIB_DIR)
vpath %.mod $(INTER_DIR)
vpath %$(OBJ_EXT) $(INTER_DIR)
vpath %.txt $(RegistryDIR)



	# complete object list
ALL_SOURCES = $(LIB_SOURCES) $(IfW_SOURCES) $(Driver_SOURCES) $(NETLIB_SOURCES)
ALL_OBJS    = $(ALL_SOURCES:.f90=$(OBJ_EXT))
ALL_OBJS    := $(ALL_OBJS:.F90=$(OBJ_EXT))
ALL_OBJS    := $(ALL_OBJS:.f=$(OBJ_EXT))


   # Rule to do everything.

all:     default
# use this for "all" for debugging: print variables:
#all:     ; $(info $$IceF_SOURCES is [${IceF_SOURCES}]) echo debugging

default: $(INTER_DIR) $(ALL_OBJS) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


###   # General rules for compliling the files.
###%.obj: %.f90
###	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)


   ### general rules for making the files ###

   # Making .o files from .f90 files
%.o: %.f90
	$(FC) $(FFLAGS) \
	-c $< \
	-o $(INTER_DIR)/$@ \
	-I $(NWTC_Lib_DIR) -J $(INTER_DIR)

   # Making .o files from .f files
%.o: %.f
	$(FC) $(FFLAGS) \
	-c $< \
	-o $(INTER_DIR)/$@ \
	-I $(NWTC_Lib_DIR) -J $(INTER_DIR)

   # Making _Types.f90 files from .txt registry files.
%_Types.f90: $(RegistryDIR)/%.txt
	$(RegistryProgram) $< -I $(RegistryNWTC);
	cp $@ ../;



   #  Dependency rules.
#NWTC Library dependency rules:
NWTC_Base$(OBJ_EXT):                $(PREC)$(OBJ_EXT)
$(SYS_FILE)$(OBJ_EXT):              NWTC_Base$(OBJ_EXT) $(PREC)$(OBJ_EXT)
NWTC_Library_Types$(OBJ_EXT):       $(SYS_FILE)$(OBJ_EXT) NWTC_Base$(OBJ_EXT)
NWTC_IO$(OBJ_EXT):                  NWTC_Library_Types$(OBJ_EXT) $(SYS_FILE)$(OBJ_EXT)
NWTC_Num$(OBJ_EXT):                 NWTC_IO$(OBJ_EXT)
ModMesh_Types$(OBJ_EXT):            NWTC_Num$(OBJ_EXT)
ModMesh$(OBJ_EXT):                  ModMesh_Types$(OBJ_EXT) NWTC_Num$(OBJ_EXT) ModMesh_Types$(OBJ_EXT)
ModMesh_Mapping$(OBJ_EXT):          ModMesh$(OBJ_EXT)  NWTC_LAPACK$(OBJ_EXT)
NWTC_Library$(OBJ_EXT):             ModMesh$(OBJ_EXT)  ModMesh_Mapping$(OBJ_EXT) NWTC_Num.o NWTC_IO$(OBJ_EXT) NWTC_LAPACK$(OBJ_EXT)
NWTC_LAPACK$(OBJ_EXT):              NWTC_Base$(OBJ_EXT)
NWTC_ScaLAPACK$(OBJ_EXT):           NWTC_Base$(OBJ_EXT) dlasrt2$(OBJ_EXT) slasrt2$(OBJ_EXT)
NWTC_NetLib$(OBJ_EXT):              NWTC_LAPACK$(OBJ_EXT) NWTC_ScaLAPACK$(OBJ_EXT)
NWTC_FFTPACK$(OBJ_EXT):					NWTC_Library$(OBJ_EXT) fftpack4.1$(OBJ_EXT)

#InflowWind dependency rules:
IfW_UniformWind_Types$(OBJ_EXT):    NWTC_Library$(OBJ_EXT)
IfW_IniformWind$(OBJ_EXT):          NWTC_Library$(OBJ_EXT) IfW_UniformWind_Types$(OBJ_EXT)
#IfW_FFWind_Types$(OBJ_EXT):         NWTC_Library$(OBJ_EXT)
#IfW_FFWind$(OBJ_EXT):               NWTC_Library$(OBJ_EXT) IfW_FFWind_Types$(OBJ_EXT)
IfW_HAWCWind_Types$(OBJ_EXT):       NWTC_Library$(OBJ_EXT)
#IfW_HAWCWind$(OBJ_EXT):             NWTC_Library$(OBJ_EXT) IfW_HAWCWind_Types$(OBJ_EXT)
InflowWind_Types$(OBJ_EXT):         NWTC_Library$(OBJ_EXT)  IfW_UniformWind_Types$(OBJ_EXT) IfW_HAWCWind_Types$(OBJ_EXT)
                                    #IfW_FFWind_Types$(OBJ_EXT)
InflowWind_Subs$(OBJ_EXT):          NWTC_Library$(OBJ_EXT)  InflowWind_Types$(OBJ_EXT)
InflowWind$(OBJ_EXT):               NWTC_Library$(OBJ_EXT)  InflowWind_Types$(OBJ_EXT) IfW_UniformWind$(OBJ_EXT)

#Driver dependency rules:
InflowWind_Driver_Types$(OBJ_EXT):  InflowWind_Types$(OBJ_EXT)
InflowWind_Driver_Subs$(OBJ_EXT):   InflowWind_Driver_Types$(OBJ_EXT)
InflowWind_Driver$(OBJ_EXT):        InflowWind$(OBJ_EXT)  InflowWind_Driver_Types$(OBJ_EXT)  InflowWind_Driver_Subs$(OBJ_EXT)



   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking driver code.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src)))


   # Cleanup afterwards.
clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*$(OBJ_EXT)
	$(DEL_CMD) $(DEST_DIR)$(PATH_SEP)$(OUTPUT_NAME)$(EXE_EXT)
#	$(DEL_CMD) $(IfW_DIR)$(PATH_SEP)IfW_FFWind_Types.f90
	$(DEL_CMD) $(IfW_DIR)$(PATH_SEP)*_Types.f90
	$(DEL_CMD) *_Types.f90
