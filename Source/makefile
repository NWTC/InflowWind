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
   Registry     = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/bin/Registry_win32.exe
   NWTC_Lib_DIR = C:/Users/bjonkman/Documents/DATA/DesignCodes/simulators/FAST/SVNdirectory/branches/BJonkman/Source/dependencies/NWTC_Library
   IfW_DIR      = .
else
   Registry     = registry.exe
   NWTC_Lib_DIR = $(FAST_DIR)/dependencies/NWTC_Library
   IfW_DIR      = $(FAST_DIR)/dependencies/InflowWind
endif


   # Name of compiler to use and flags to use.
FC      = gfortran

FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -DNO_MESHMAPPING
#FFLAGS  = -O2 -m$(BITS) -fbacktrace -ffree-line-length-none -x f95-cpp-input -C -DNO_MESHMAPPING
#FFLAGS = -g -O0 -v -da -Q -fbacktrace -ffree-line-length-none -x f95-cpp-input -DNO_MESHMAPPING

LDFLAGS = -O2 -m$(BITS) -fbacktrace

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
      # Linux
   DEL_CMD   = rm -f
   EXE_EXT   = _glin$(BITS)
   INTER_DIR = Obj_lin$(BITS)
   MD_CMD    = @mkdir -p
   OBJ_EXT   = .o
   PATH_SEP  = /
   SYS_FILE  = SysGnuLinux
endif


   # Source files (by module)

LIB_SOURCES =           \
	$(PREC).f90          \
	NWTC_Base.f90        \
	$(SYS_FILE).f90      \
	NWTC_Library_Types.f90 \
	NWTC_IO.f90          \
	NWTC_Num.f90         \
	ModMesh_Types.f90    \
	ModMesh.f90          \
	NWTC_Library.f90


IfW_SOURCES =            \
	IfW_FFWind_Types.f90  \
	IfW_FFWind.f90        \
	IfW_HHWind_Types.f90  \
	IfW_HHWind.f90        \
	InflowWind_Subs.f90   \
	InflowWind_Types.f90  \
	InflowWind.f90

DRIVER_SOURCES =            \
	InflowWind_Driver.f90        \
	InflowWind_Driver_Types.f90  \
	InflowWind_Driver_Subs.f90



vpath %.f90 $(NWTC_Lib_DIR) $(IfW_DIR) $(InflowWind_Driver_DIR)
vpath %.mod $(INTER_DIR)
vpath %.obj $(INTER_DIR)
vpath %.txt $(IfW_DIR)



ALL_SOURCES = $(LIB_SOURCES) $(IfW_SOURCES) $(DRIVER_SOURCES)
tmp_objs1   = $(ALL_SOURCES:.f90=.obj)
tmp_objs2   = $(tmp_objs1:.F90=.obj)
ALL_OBJS    = $(tmp_objs2:.f=.obj)


   # Rule to do everything.

all:     default
# use this for "all" for debugging: print variables:
#all:     ; $(info $$IceF_SOURCES is [${IceF_SOURCES}]) echo debugging
#
default:  $(INTER_DIR) $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT)


   # General rules for compliling the files.

%.obj: %.f90
	$(FC) -I $(INTER_DIR) $(FFLAGS) -c $< -o $(INTER_DIR)/$@ -J $(INTER_DIR) -B $(INTER_DIR)


   # General rules for creating _Types.f90 files from Registry files.


IfW_HHWind_Types.f90:
	$(Registry) Reg-IfW_HHWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)

IfW_FFWind_Types.f90:
	$(Registry) Reg-IfW_FFWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)

InflowWind_Types.f90:
	$(Registry) Reg-InflowWind.txt -I $(NWTC_Lib_DIR) -I $(IfW_DIR)


   #  Dependency rules.
#NWTC Library dependency rules:
NWTC_Base.obj:                $(PREC).obj
$(SYS_FILE).obj:              NWTC_Base.obj
NWTC_Library_Types.obj:       $(SYS_FILE).obj
NWTC_IO.obj:                  NWTC_Library_Types.obj
NWTC_Num.obj:                 NWTC_IO.obj
ModMesh_Types.obj:            NWTC_Num.obj
ModMesh.obj:                  ModMesh_Types.obj
NWTC_Library.obj:             ModMesh.obj  ModMesh.obj


#InflowWind dependency rules:
IfW_HHWind_Types.obj:         NWTC_Library.obj
IfW_HHWind.obj:               IfW_HHWind_Types.obj
IfW_FFWind_Types.obj:         NWTC_Library.obj
IfW_FFWind.obj:               IfW_FFWind_Types.obj
InflowWind_Types.obj:         NWTC_Library.obj  IfW_FFWind_Types.obj  IfW_HHWind_Types.obj
InflowWind_Subs.obj:          InflowWind_Types.obj IfW_FFWind.obj  IfW_HHWind.obj
InflowWind.obj:               InflowWind_Subs.obj

#Driver dependency rules:
InflowWind_Driver_Types.obj:  InflowWind_Types.obj
InflowWind_Driver_Subs.obj:   InflowWind_Driver_Types.obj
InflowWind_Driver.obj:        InflowWind.obj  InflowWind_Driver_Types.obj  InflowWind_Driver_Subs.obj


#$(OUTPUT_NAME)$(EXE_EXT): InflowWind_Driver.obj

   # Make sure the destination directory for the intermediate files exist.

$(INTER_DIR):
	$(MD_CMD) $(INTER_DIR)


   # For linking driver code.

$(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT): $(ALL_OBJS) | $(INTER_DIR)
	$(FC) $(LDFLAGS) -I $(INTER_DIR) -o $(DEST_DIR)/$(OUTPUT_NAME)$(EXE_EXT) \
	$(foreach src, $(ALL_OBJS), $(addprefix $(INTER_DIR)/,$(src)))

   # Cleanup afterwards.

clean:
	$(DEL_CMD) $(INTER_DIR)$(PATH_SEP)*.mod $(INTER_DIR)$(PATH_SEP)*.obj
	$(DEL_CMD) $(DEST_DIR)$(PATH_SEP)$(OUTPUT_NAME)$(EXE_EXT)
	$(DEL_CMD) $(IfW_DIR)$(PATH_SEP)InflowWind_Types.f90
	$(DEL_CMD) $(IfW_DIR)$(PATH_SEP)IfW_FFWind_Types.f90
	$(DEL_CMD) $(IfW_DIR)$(PATH_SEP)IfW_HHWind_Types.f90
#	rmdir $(DEST_DIR);

