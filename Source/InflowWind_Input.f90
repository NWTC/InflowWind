!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2013  National Renewable Energy Laboratory
!
!    This file is part of InflowWind.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!**********************************************************************************************************************************
! File last committed: $Date: 2014-08-12 09:45:18 -0600 (Tue, 12 Aug 2014) $
! (File) Revision #: $Rev: 495 $
! URL: $HeadURL$
!**********************************************************************************************************************************
MODULE InflowWind_Input

      ! This MODULE stores variables used for input.

   USE                              NWTC_Library
   USE                              InflowWind_Types
!   USE                              InflowWind_Output
   IMPLICIT                         NONE

   PRIVATE :: CleanupEchoFile

CONTAINS


!====================================================================================================
SUBROUTINE PrintBadChannelWarning(NUserOutputs, UserOutputs , foundMask, ErrStat, ErrMsg )
!     The routine prints out warning messages if the user has requested invalid output channel names
!     The errstat is set to ErrID_Warning if any element in foundMask is .FALSE.
!----------------------------------------------------------------------------------------------------
   INTEGER(IntKi),                     INTENT(IN   )  :: NUserOutputs         !< Number of user-specified output channels
   CHARACTER(10),                      INTENT(IN   )  :: UserOutputs(:)       !< An array holding the names of the requested output channels.
   LOGICAL,                            INTENT(IN   )  :: foundMask(:)         !< A mask indicating whether a user requested channel belongs to a module's output channels.
   INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat              !< returns a non-zero value when an error occurs
   CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg               !< Error message if ErrStat /= ErrID_None


  INTEGER(IntKi)                                      :: I                    !< Generic loop counter

   ErrStat = ErrID_None
   ErrMsg  = ''

   DO I = 1, NUserOutputs
      IF (.NOT. foundMask(I)) THEN
         ErrMsg  = ' A requested output channel is invalid'
         CALL ProgWarn( 'The requested output channel is invalid: ' // UserOutputs(I) )
         ErrStat = ErrID_Warn
      END IF
   END DO



END SUBROUTINE PrintBadChannelWarning


!====================================================================================================
SUBROUTINE CleanupEchoFile( EchoFlag, UnEcho)
!     The routine cleans up the module echo file and resets the NWTC_Library, reattaching it to
!     any existing echo information
!----------------------------------------------------------------------------------------------------
   LOGICAL,                       INTENT( IN    )   :: EchoFlag             ! local version of echo flag
   INTEGER,                       INTENT( IN    )   :: UnEcho               !  echo unit number


      ! Close this module's echo file

   IF ( EchoFlag ) THEN
    CLOSE(UnEcho)
   END IF



END SUBROUTINE CleanupEchoFile




!====================================================================================================
SUBROUTINE InflowWindInput_GetInput( InitInput, InitOutput, ParamData, ErrStat, ErrMsg )
!     This public subroutine reads the input required for InflowWind from the file whose name is an
!     input parameter.
!----------------------------------------------------------------------------------------------------


      ! Passed variables
   TYPE(InflowWind_InitInputType),     INTENT(IN   )  :: InitInput            !< The data for initialization
   TYPE(InflowWind_InitOutputType),    INTENT(INOUT)  :: InitOutput           !< The output data information
   TYPE(InflowWind_ParameterType),     INTENT(INOUT)  :: ParamData            !< The parameter information that does not change
   INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat              !< Returned error status  from this subroutine 
   CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg               !< Returned error message from this subroutine


      ! Local variables
!   INTEGER(IntKi)                                     :: I                    !< Generic loop counter

   LOGICAL                                            :: EchoFlag             !< Are we echoing info?
   INTEGER(IntKi)                                     :: UnIn                 !< Unit number for the input file
   INTEGER(IntKi)                                     :: UnEchoLocal          !< The local unit number for this module's echo file
   CHARACTER(1024)                                    :: TmpPath              !< Temporary storage for relative path name
   CHARACTER(1024)                                    :: TmpFmt               !< Temporary storage for format statement
   CHARACTER(1024)                                    :: FileName             !< Name of InflowWind input file
   CHARACTER(1024)                                    :: RootName             !< Root name of InflowWind input file
   CHARACTER(1024)                                    :: EchoFile             !< Name of InflowWind echo file
   CHARACTER(35)                                      :: Frmt                 !< Output format for logical parameters. (matches NWTC Subroutine Library format)
 

      ! Temoporary messages
   INTEGER(IntKi)                                     :: TmpErrStat
   CHARACTER(LEN(ErrMsg))                             :: TmpErrMsg


      ! Initialize local data

   UnEchoLocal    = -1
   Frmt           = "( 2X, L11, 2X, A, T30, ' - ', A )"
   ErrStat        = ErrID_None
   ErrMsg         = ""
   EchoFlag       = .FALSE.  ! initialize for error handling (cleanup() routine)


      ! Set a local variable for the filename (cleaner looking code)
   FileName = TRIM(ParamData%InputFileName)
   EchoFile = TRIM(ParamData%EchoName) 


   !-------------------------------------------------------------------------------------------------
   ! Open the file
   !-------------------------------------------------------------------------------------------------

   CALL GetNewUnit( UnIn, TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )

   CALL OpenFInpFile( UnIn, TRIM(FileName), TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF


   !CALL WrScr( 'Opening InflowWind input file:  '//FileName )


   !-------------------------------------------------------------------------------------------------
   ! File header
   !-------------------------------------------------------------------------------------------------

   CALL ReadCom( UnIn, FileName, 'InflowWind input file header line 1', TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

   CALL ReadCom( UnIn, FileName, 'InflowWind input file header line 2', TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF


     ! Echo Input Files.

   CALL ReadVar ( UnIn, FileName, EchoFLag, 'Echo', 'Echo Input', TmpErrStat, TmpErrMsg )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! If we are Echoing the input then we should re-read the first three lines so that we can echo them
      ! using the NWTC_Library routines.  The echoing is done inside those routines via a global variable
      ! which we must store, set, and then replace on error or completion.

   IF ( EchoFlag ) THEN

      CALL OpenEcho ( UnEchoLocal, TRIM(EchoFile), TmpErrStat, TmpErrMsg )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
      IF (ErrStat >= AbortErrLev) THEN
         CALL CleanUp()
         RETURN
      END IF

      REWIND(UnIn)


         ! The input file was already successfully read through up to this point, so we shouldn't have any read
         ! errors in the first four lines.  So, we won't worry about checking the error status here.

      CALL ReadCom( UnIn, FileName, 'InflowWind input file header line 1', TmpErrStat, TmpErrMsg, UnEchoLocal )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )

      CALL ReadCom( UnIn, FileName, 'InflowWind input file header line 2', TmpErrStat, TmpErrMsg, UnEchoLocal )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )

      CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )

         ! Echo Input Files.

      CALL ReadVar ( UnIn, FileName, EchoFlag, 'Echo', 'Echo the input file data', TmpErrStat, TmpErrMsg, UnEchoLocal )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )

   END IF



   !-------------------------------------------------------------------------------------------------
   !> Read general section with wind type, direction, and output point list (applies to all wind types)
   !-------------------------------------------------------------------------------------------------


      ! Read WindType
   CALL ReadVar( UnIn, FileName, ParamData%WindType, 'WindType', 'switch for wind file type (1=steady; '// &
               '2=uniform; 3=binary TurbSim FF; 4=binary Bladed-style FF; 5=HAWC format; 6=User defined)', &
               TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


      ! Read PropogationDir
   CALL ReadVar( UnIn, FileName, ParamData%PropogationDir, 'PropogationDir', 'Direction of wind propogation '// &
               '(meteoroligical direction)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


      ! Read the number of points for the wind velocity output
   CALL ReadVar( UnIn, Filename, ParamData%NWindVel, 'NWindVel', 'Number of points to output the wind velocity (0 to 9)', &
               TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

      ! Before proceeding, make sure that NWindVel makes sense
   IF ( ParamData%NWindVel < 0 .OR. ParamData%NwindVel > 9 ) THEN
      CALL SetErrStat( ErrID_Fatal, 'NWindVel must be greater than or equal to zero and less than 10.', ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
      CALL CleanUp()
      RETURN
   ELSE

      ! Allocate space for the output location arrays:
      CALL AllocAry( ParamData%WindVxiList, ParamData%NWindVel, 'WindVxiList', TmpErrStat, TmpErrMsg )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
      CALL AllocAry( ParamData%WindVyiList, ParamData%NWindVel, 'WindVyiList', TmpErrStat, TmpErrMsg )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
      CALL AllocAry( ParamData%WindVziList, ParamData%NWindVel, 'WindVziList', TmpErrStat, TmpErrMsg )
      CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
      IF (ErrStat >= AbortErrLev) THEN
         CALL CleanUp()
         RETURN
      ENDIF 
   ENDIF

      ! Read in the values of WindVxiList
   CALL ReadAry( UnIn, Filename, ParamData%WindVxiList, ParamData%NWindVel, 'WindVxiList', &
               'List of coordinates in the inertial X direction (m)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 
   
      ! Read in the values of WindVxiList
   CALL ReadAry( UnIn, Filename, ParamData%WindVyiList, ParamData%NWindVel, 'WindVyiList', &
               'List of coordinates in the inertial Y direction (m)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 
   
      ! Read in the values of WindVziList
   CALL ReadAry( UnIn, Filename, ParamData%WindVziList, ParamData%NWindVel, 'WindVziList', &
               'List of coordinates in the inertial Z direction (m)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

   
   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for Steady Wind Conditions [used only for WindType = 1]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF


      ! Read HWindSpeed
   CALL ReadVar( UnIn, FileName, ParamData%HWindSpeed, 'HWindSpeed', 'Horizontal windspeed', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

      ! Read RefHt
   CALL ReadVar( UnIn, FileName, ParamData%RefHt, 'RefHt', 'Reference height for horizontal wind speed', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

      ! Read PLexp
   CALL ReadVar( UnIn, FileName, ParamData%PLexp, 'PLexp', 'Power law exponent', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for Uniform wind file [used only for WindType = 2]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

!FIXME: change name from HH to UniformWind or something
      ! Read HHWindFile
   CALL ReadVar( UnIn, FileName, ParamData%HHWind%WindFileName, 'WindFileName', &
               'Filename of time series data for uniform wind field', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

      ! Read RefHt
   CALL ReadVar( UnIn, FileName, ParamData%RefHt, 'RefHt', 'Reference height for uniform wind file', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for Binary TurbSim Full-Field files [used only for WindType = 3]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read FFWindFile
   CALL ReadVar( UnIn, FileName, ParamData%FFWind%WindFileName, 'FileName', &
               'Name of the TurbSim full field wind file to use (.bts)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for Binary Bladed-style Full-Field files [used only for WindType = 4]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

!FIXME: change when have the bladedstyle module assembled
      ! Read BladedStyle%WindFileName
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
!   CALL ReadVar( UnIn, FileName, ParamData%BladedStyle%WindFileName, 'FileName', &
!               'Name of the TurbSim full field wind file to use (.bts)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

!FIXME: change when have the bladedstyle module assembled
      ! Read TowerFileFlag
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
!   CALL ReadVar( UnIn, FileName, ParamData%BladedStyle%TowerFileFlag, 'TowerFileFlag', &
!               'Have tower file (.twr) [flag]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 


   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for coherent turbulence [used only for WindType = 3 or 4]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read CT_Flag
   CALL ReadVar( UnIn, FileName, ParamData%CT_Flag, 'CT_Flag', &
               'Use coherent turbulence', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

!FIXME: change when have the CT module assembled
      ! Read CTWind%WindFileName
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
!   CALL ReadVar( UnIn, FileName, ParamData%CTWind%WindFileName, 'FileName', &
!               'Name of coherent turbulence file', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 

!FIXME: change when have the CT module assembled
      ! Read CTWind%PathName
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
!   CALL ReadVar( UnIn, FileName, ParamData%CTWind%PathName, 'TowerFileFlag', &
!               'Path to coherent turbulence binary files', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput')
   IF (ErrStat >= AbortErrLev) THEN
      CALL CleanUp()
      RETURN
   ENDIF 



   !-------------------------------------------------------------------------------------------------
   !> Read the _Parameters for HAWC-formatted binary files [used only for WindType = 5]_ section
   !-------------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%FileName_u
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%FileName_u, 'FileName_u', &
               'Name of the file containing the u-component fluctuating wind', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%FileName_v
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%FileName_v, 'FileName_v', &
               'Name of the file containing the v-component fluctuating wind', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%FileName_w
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%FileName_w, 'FileName_w', &
               'Name of the file containing the w-component fluctuating wind', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%nx
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%nx, 'nx', &
               'Number of grids in the x direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%ny
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%ny, 'ny', &
               'Number of grids in the y direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%nz
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%nz, 'nz', &
               'Number of grids in the z direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%dx
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%dx, 'dx', &
               'Number of grids in the x direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%dy
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%dy, 'dy', &
               'Number of grids in the y direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%dz
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%dz, 'dz', &
               'Number of grids in the z direction (in the 3 files above)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%RefHt
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%RefHt, 'RefHt', &
               'Reference (hub) height of the grid', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF



      !----------------------------------------------------------------------------------------------
      !> Read the _Scaling parameters for turbulence (HAWC-format files) [used only for WindType = 5]_ subsection
      !----------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%ScaleMethod
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%ScaleMethod, 'ScaleMethod', &
               'Turbulence scaling method [0=none, 1=direct scaling, 2= calculate scaling '// &
               'factor based on a desired standard deviation]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SFx
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SFx, 'SFx', &
               'Turbulence scaling factor for the x direction [ScaleMethod=1]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SFy
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SFy, 'SFy', &
               'Turbulence scaling factor for the y direction [ScaleMethod=1]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SFz
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SFz, 'SFz', &
               'Turbulence scaling factor for the z direction [ScaleMethod=1]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SigmaFx
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SigmaFx, 'SigmaFx', &
               'Turbulence standard deviation to calculate scaling from in x direction [ScaleMethod=2]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SigmaFy
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SigmaFy, 'SigmaFy', &
               'Turbulence standard deviation to calculate scaling from in y direction [ScaleMethod=2]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%SigmaFz
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%SigmaFz, 'SigmaFz', &
               'Turbulence standard deviation to calculate scaling from in z direction [ScaleMethod=2]', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

!FIXME:  TStart has no comment
      ! Read HAWCWind%TStart
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%TStart, 'TStart', &
               '', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

!FIXME:  TStart has no comment
      ! Read HAWCWind%TEnd
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%TEnd, 'TEnd', &
               '', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF


      !----------------------------------------------------------------------------------------------
      !> Read the _Mean wind profile paramters (added to HAWC-format files) [used only for WindType = 5]_ subsection
      !----------------------------------------------------------------------------------------------

      ! Section separator line
   CALL ReadCom( UnIn, FileName, 'InflowWind input file separator line', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%URef
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%URef, 'URef', &
               'Mean u-component wind speed at the reference height', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%WindProfileType
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%WindProfileType, 'WindProfileType', &
               'Wind profile type ("LOG"=logarithmic, "PL"=power law, or "UD"=user defined)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%PLExp
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%PLExp, 'PLExp', &
               'Power law exponent (used for PL wind profile type only)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF

      ! Read HAWCWind%Z0
   CALL ReadVar( UnIn, FileName, ParamData%HAWCWind%Z0, 'Z0', &
               'Surface roughness length (used for LOG wind profile type only)', TmpErrStat, TmpErrMsg, UnEchoLocal )
   CALL SetErrStat( TmpErrStat, TmpErrMsg, ErrStat, ErrMsg, 'InflowWindInput_GetInput' )
   IF (ErrStat >= AbortErrLev) THEN
      CALL Cleanup()
      RETURN
   END IF




!FIXME: read the outlist


   !-------------------------------------------------------------------------------------------------
   ! This is the end of the input file
   !-------------------------------------------------------------------------------------------------

   CALL Cleanup()

   RETURN

   CONTAINS
      !..............................
      SUBROUTINE Cleanup()


            ! Close input file
         CLOSE ( UnIn )

            ! Cleanup the Echo file and global variables
         CALL CleanupEchoFile( InitInput%Echo, UnEchoLocal )


      END SUBROUTINE Cleanup

END SUBROUTINE InflowWindInput_GetInput


!====================================================================================================
SUBROUTINE InflowWindInput_ProcessInitData( InitInput, ErrStat, ErrMsg )
!     This private subroutine verifies the input required for InflowWind is correctly specified.
!----------------------------------------------------------------------------------------------------


      ! Passed variables

   TYPE(InflowWind_InitInputType),     INTENT(INOUT)  :: InitInput            !< The data for initialization
   INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat              !< Error status  from this subroutine
   CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg               !< Error message from this subroutine


      ! Temporary variables
   INTEGER(IntKi)                                     :: TmpErrStat           !< Temporary error status  for subroutine and function calls
   CHARACTER(LEN(ErrMsg))                             :: TmpErrMsg            !< Temporary error message for subroutine and function calls

      ! Local variables


   !----------------------------

      ! Initialize ErrStat

   ErrStat = ErrID_None
   ErrMsg  = ""

END SUBROUTINE InflowWindInput_ProcessInitData


END MODULE InflowWind_Input
