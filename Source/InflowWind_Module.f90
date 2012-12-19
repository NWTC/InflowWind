!**********************************************************************************************************************************
! This module is used to read and process the (undisturbed) inflow winds.  It must be initialized
! using InflowWind_Init() with the name of the file, the file type, and possibly reference height and
! width (depending on the type of wind file being used).  This module calls appropriate routines
! in the wind modules so that the type of wind becomes seamless to the user.  IfW_End()
! should be called when the program has finshed.
!
! Data are assumed to be in units of meters and seconds.  Z is measured from the ground (NOT the hub!).
!
!  7 Oct 2009    Initial Release with AeroDyn 13.00.00         B. Jonkman, NREL/NWTC
! 14 Nov 2011    v1.00.01b-bjj                                 B. Jonkman
!  1 Aug 2012    v1.01.00a-bjj                                 B. Jonkman
! 10 Aug 2012    v1.01.00b-bjj                                 B. Jonkman
!    Dec 2012    v2.00.00a-adp   conversion to Framework       A. Platt
!
!..................................................................................................................................
! Files with this module:
!  InflowWind_Subs.f90
!  InflowWind.txt       -- InflowWind_Types will be auto-generated based on the descriptions found in this file.
!  
!..................................................................................................................................
! LICENSING
! Copyright (C) 2012  National Renewable Energy Laboratory
!
!    This file is part of InflowWind.
!
!    InflowWind is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
!    published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!    
!    You should have received a copy of the GNU General Public License along with InflowWind.  
!    If not, see <http://www.gnu.org/licenses/>.
!    
!**********************************************************************************************************************************
MODULE InflowWind


   USE                              SharedInflowDefs
!   USE                              InflowWind_Types   !FIXME: this file needs to be made.
   USE                              NWTC_Library
   USE                              WindFile_Types
      
      !-------------------------------------------------------------------------------------------------
      ! The included wind modules
      !-------------------------------------------------------------------------------------------------

   USE                              FFWind               ! full-field binary wind files
   USE                              HHWind               ! hub-height text wind files
   USE                              FDWind               ! 4-D binary wind files
   USE                              CTWind               ! coherent turbulence from KH billow - binary file superimposed on another wind type
   USE                              UserWind             ! user-defined wind module
   USE                              HAWCWind             ! full-field binary wind files in HAWC format


      !-------------------------------------------------------------------------------------------------
      ! The subroutines
      !-------------------------------------------------------------------------------------------------

   USE                              InflowWind_Subs      ! all the subroutines live here now.



   
   IMPLICIT NONE
   PRIVATE

   INTEGER(IntKi), PARAMETER            :: DataFormatID = 1           ! Update this value if the data types change (used in IfW_Pack())
!FIXME: tie this to InitOut as well.
   TYPE(ProgDesc), PARAMETER            :: IfW_ProgDesc = ProgDesc( 'InflowWind', 'v1.00.00', '27-Dec-2012' )

!   CHARACTER(99),PARAMETER        :: InflowWindVer = 'InflowWind (v1.01.00b-bjj, 10-Aug-2012)'

   
      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: IfW_Init                                 ! Initialization routine
   PUBLIC :: IfW_End                                  ! Ending routine (includes clean up)
   
!   PUBLIC :: InflowWind_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
!   PUBLIC :: InflowWind_CalcOutput                     ! Routine for computing outputs
!   
!   PUBLIC :: InflowWind_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual
!   PUBLIC :: InflowWind_CalcContStateDeriv             ! Tight coupling routine for computing derivatives of continuous states
!   PUBLIC :: InflowWind_UpdateDiscState                ! Tight coupling routine for updating discrete states
!      
!   PUBLIC :: InflowWind_JacobianPInput                 ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations all with respect to the inputs (u)
!   PUBLIC :: InflowWind_JacobianPContState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations all with respect to the continuous states (x)
!   PUBLIC :: InflowWind_JacobianPDiscState             ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations all with respect to the discrete states (xd)
!   PUBLIC :: InflowWind_JacobianPConstrState           ! Routine to compute the Jacobians of the output (Y), continuous- (X), discrete- (Xd), and constraint-state (Z) equations all with respect to the constraint states (z)
!   
!   PUBLIC :: InflowWind_Pack                           ! Routine to pack (save) data into one array of bytes
!   PUBLIC :: InflowWind_Unpack                         ! Routine to unpack an array of bytes into data structures usable by the module
   
!-=- Original bits follow -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


   !-------------------------------------------------------------------------------------------------
   ! Private internal variables
   !-------------------------------------------------------------------------------------------------

!FIXME: handle this differently -- should be allocated by the library function to get an open unit number
      ! store as parameter in parametertype
   INTEGER                        :: UnWind   = 91          ! The unit number used for wind inflow files


   !-------------------------------------------------------------------------------------------------
   ! Definitions of public types and routines
   !-------------------------------------------------------------------------------------------------

!FIXME: should not be public anymore
   PUBLIC                         :: InflowWind_GetVelocity    ! function to get wind speed at point in space and time

!FIXME: not public anymore. may not even exist when done.
!   PUBLIC                         :: InflowWind_ADhack_diskVel ! used to keep old AeroDyn functionality--remove soon!
!   PUBLIC                         :: InflowWind_ADhack_DIcheck ! used to keep old AeroDyn functionality--remove soon!

!FIXME: not public anymore.
!   PUBLIC                         :: InflowWind_LinearizePerturbation !used for linearization; should be modified

!!----Removed during conversion to new framework: may put back in as part of OtherStates
!!       PUBLIC                         :: InflowWind_GetMean        ! function to get the mean wind speed at a point in space
!!       PUBLIC                         :: InflowWind_GetStdDev      ! function to calculate standard deviation at a point in space
!!       PUBLIC                         :: InflowWind_GetTI          ! function to get TI at a point in space


CONTAINS
!====================================================================================================
!  SUBROUTINE ModName_Init( InitData, InputGuess, ParamData, ContStates, DiscStates, ConstrStateGuess, OtherStates, &
!                            OutData, Interval, ErrStat, ErrMsg )
   SUBROUTINE IfW_Init( InitData, ParamData, Interval, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps. 
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!----------------------------------------------------------------------------------------------------
!  Open and read the wind files, allocating space for necessary variables


         ! Initialization data and guesses

      TYPE( IfW_InitInputType ),          INTENT(IN   )  :: InitData          ! Input data for initialization
!      TYPE(ModName_InputType),           INTENT(  OUT)  :: InputGuess        ! An initial guess for the input; the input mesh must be defined
!      TYPE(ModName_ParameterType),       INTENT(  OUT)  :: ParamData         ! Parameters      
      TYPE( Ifw_ParameterType ),          INTENT(  OUT)  :: ParamData         ! Parameters
!      TYPE(ModName_ContinuousStateType), INTENT(  OUT)  :: ContStates        ! Initial continuous states
!      TYPE(ModName_DiscreteStateType),   INTENT(  OUT)  :: DiscStates        ! Initial discrete states
!      TYPE(ModName_ConstraintStateType), INTENT(  OUT)  :: ConstrStateGuess  ! Initial guess of the constraint states
!      TYPE(ModName_OtherStateType),      INTENT(  OUT)  :: OtherStates       ! Initial other/optimization states            
!      TYPE(ModName_OutputType),          INTENT(  OUT)  :: OutData           ! Initial output (outputs are not calculated; only the output mesh is initialized)
      REAL(DbKi),                         INTENT(INOUT)  :: Interval          ! Coupling interval in seconds: InflowWind does not change this.


         ! Error Handling

      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Local variables

      TYPE(HH_Info)                                      :: HHInitInfo
      TYPE(CT_Backgr)                                    :: BackGrndValues


!NOTE: It isn't entirely clear what the purpose of Height is. Does it sometimes occur that Height  /= ParamData%ReferenceHeight???
      REAL(ReKi)                                         :: Height      ! Retrieved from FF
      REAL(ReKi)                                         :: HalfWidth   ! Retrieved from FF
      CHARACTER(1024)                                    :: FileName

!NOTE: I may need to revamp how data is passed to the lower modules. Might need to do that before going any further.


         ! Initialize ErrStat
         
      ErrStat = ErrID_None         
      ErrMsg  = ""               
      
      
!
!FIXME:
!         ! Define parameters here:
!         
!      !ParamData%DT     = Interval             ! InflowWind does not dictate the time interval.
                                                ! It only responds to the current time.
!      !ParamData%       = 
!
!
!FIXME: no states -- except maybe otherstates.
!         ! Define initial states here
!
!      !ContStates%      = 
!      !DiscStates%      = 
!      !ConstrStateGuess%= 
!      !OtherStates%     = 
!
!
!FIXME: I think there are no initial guesses
!         ! Define initial guess for the input here:
!
!      !InputGuess%      = 
!
!
!FIXME: setup the output data matrix.
!         ! Define output initializations (set up mesh) here:
!      !OutData%        =         
         

      ! check to see if we are already initialized

   IF ( ParamData%Initialized ) THEN
      CALL WrScr( ' Wind inflow has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
         ! Copy things into the ParamaterType -- InitData may not exist later and isn't accessable in some routines.
      ParamData%WindFileType = InitData%WindFileType
      ParamData%WindFileName = InitData%WindFileName
!FIXME: this is temporary and should be removed once the Wind modules are done.
      FileName = InitData%WindFileName
      CALL NWTC_Init()
      CALL DispNVD( IfW_ProgDesc )

   END IF

   !-------------------------------------------------------------------------------------------------
   ! Get default wind type, based on file name, if requested. Otherwise store what we are given for the type
   !-------------------------------------------------------------------------------------------------
   IF ( InitData%WindFileType == DEFAULT_Wind ) THEN
      CALL GetWindType( ParamData, ErrStat, ErrMsg )
   ELSE
      ParamData%WindFileType = InitData%WindFileType
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Check for coherent turbulence file (KH superimposed on a background wind file)
   ! Initialize the CTWind module and initialize the module of the other wind type.
   !-------------------------------------------------------------------------------------------------

   IF ( ParamData%WindFileType == CTP_Wind ) THEN

      CALL CT_Init(UnWind, ParamData%WindFileName, BackGrndValues, ErrStat)
      IF (ErrStat /= 0) THEN
         CALL IfW_End( ParamData, ErrStat )
         ParamData%WindFileType = Undef_Wind
         ErrStat  = 1
         RETURN
      END IF

!FIXME: check this
      ParamData%WindFileName = BackGrndValues%WindFile
      ParamData%WindFileType = BackGrndValues%WindFileType
!      CT_Flag  = BackGrndValues%CoherentStr
      ParamData%CT_Flag  = BackGrndValues%CoherentStr    ! This might be wrong

   ELSE

!      CT_Flag  = .FALSE.
      ParamData%CT_Flag  = .FALSE.

   END IF

   !-------------------------------------------------------------------------------------------------
   ! Initialize based on the wind type
   !-------------------------------------------------------------------------------------------------

   SELECT CASE ( ParamData%WindFileType )

      CASE (HH_Wind)

         HHInitInfo%ReferenceHeight = InitData%ReferenceHeight
         HHInitInfo%Width           = InitData%Width

         CALL HH_Init( UnWind, ParamData%WindFileName, HHInitInfo, ErrStat )

!        IF (CT_Flag) CALL CT_SetRefVal(FileInfo%ReferenceHeight, 0.5*FileInfo%Width, ErrStat)
         IF (ErrStat == 0 .AND. ParamData%CT_Flag) CALL CT_SetRefVal(InitData%ReferenceHeight, REAL(0.0, ReKi), ErrStat)


      CASE (FF_Wind)

         CALL FF_Init( UnWind, ParamData%WindFileName, ErrStat )


            ! Set CT parameters
         IF ( ErrStat == 0 .AND. ParamData%CT_Flag ) THEN
            Height     = FF_GetValue('HubHeight', ErrStat)
            IF ( ErrStat /= 0 ) Height = InitData%ReferenceHeight

            HalfWidth  = 0.5*FF_GetValue('GridWidth', ErrStat)
            IF ( ErrStat /= 0 ) HalfWidth = 0

            CALL CT_SetRefVal(Height, HalfWidth, ErrStat)
         END IF


      CASE (UD_Wind)

         CALL UsrWnd_Init(ErrStat)


      CASE (FD_Wind)

         CALL FD_Init(UnWind, ParamData%WindFileName, InitData%ReferenceHeight, ErrStat)

      CASE (HAWC_Wind)

         CALL HW_Init( UnWind, ParamData%WindFileName, ErrStat )

      CASE DEFAULT

         CALL WrScr(' Error: Undefined wind type in WindInflow_Init()' )
         ErrStat = 1
         RETURN

   END SELECT


      ! check error status. If no error, set flag to indicate we are initialized.

   IF ( ErrStat /= 0 ) THEN
      ParamData%Initialized = .FALSE.
      ParamData%WindFileType    = Undef_Wind
      ErrStat               = 1         !FIXME: change the error status to the framework convention
      CALL IfW_End( ParamData, ErrStat )  !Just in case we've allocated something
   ELSE
      ParamData%Initialized = .TRUE.
   END IF

   RETURN

END SUBROUTINE IfW_Init
!====================================================================================================
!FIXME: this becomes part of InflowWind_CalcOutput
FUNCTION InflowWind_GetVelocity(ParamData, Time, InputPosition, ErrStat)
! Get the wind speed at a point in space and time
!----------------------------------------------------------------------------------------------------

      ! passed variables
   TYPE(IfW_ParameterType),               INTENT(IN   )  :: ParamData
   REAL(ReKi),       INTENT(IN)  :: Time
   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
   INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise

      ! local variables
   TYPE(InflIntrpOut)            :: InflowWind_GetVelocity     ! U, V, W velocities
   TYPE(InflIntrpOut)            :: CTWindSpeed             ! U, V, W velocities to superimpose on background wind


   ErrStat = 0

   SELECT CASE ( ParamData%WindFileType )
      CASE (HH_Wind)
         InflowWind_GetVelocity = HH_GetWindSpeed(     Time, InputPosition, ErrStat )

      CASE (FF_Wind)
         InflowWind_GetVelocity = FF_GetWindSpeed(     Time, InputPosition, ErrStat )

      CASE (UD_Wind)
         InflowWind_GetVelocity = UsrWnd_GetWindSpeed( Time, InputPosition, ErrStat )

      CASE (FD_Wind)
         InflowWind_GetVelocity = FD_GetWindSpeed(     Time, InputPosition, ErrStat )

      CASE (HAWC_Wind)
         InflowWind_GetVelocity = HW_GetWindSpeed(     Time, InputPosition, ErrStat )

      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in InflowWind_GetVelocity(). ' &
                   //'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1
         InflowWind_GetVelocity%Velocity(:) = 0.0

   END SELECT


   IF (ErrStat /= 0) THEN

      InflowWind_GetVelocity%Velocity(:) = 0.0

   ELSE

         ! Add coherent turbulence to background wind

      IF (ParamData%CT_Flag) THEN

         CTWindSpeed = CT_GetWindSpeed(Time, InputPosition, ErrStat)
         IF (ErrStat /=0 ) RETURN

         InflowWind_GetVelocity%Velocity(:) = InflowWind_GetVelocity%Velocity(:) + CTWindSpeed%Velocity(:)

      ENDIF

   ENDIF

END FUNCTION InflowWind_GetVelocity
!====================================================================================================
!FIXME: rename as per framework.
SUBROUTINE IfW_End( ParamData, ErrStat )
! Clean up the allocated variables and close all open files.  Reset the initialization flag so
! that we have to reinitialize before calling the routines again.
!----------------------------------------------------------------------------------------------------
   USE WindFile_Types

   TYPE(IfW_ParameterType),         INTENT(INOUT)  :: ParamData   ! Parameters that typically don't change
   INTEGER, INTENT(OUT)       :: ErrStat     !bjj: do we care if there's an error on cleanup?


      ! Close the wind file, if it happens to be open

   CLOSE( UnWind )


      ! End the sub-modules (deallocates their arrays and closes their files):

   SELECT CASE ( ParamData%WindFileType )

      CASE (HH_Wind)
         CALL HH_Terminate(     ErrStat )

      CASE (FF_Wind)
         CALL FF_Terminate(     ErrStat )

      CASE (UD_Wind)
         CALL UsrWnd_Terminate( ErrStat )

      CASE (FD_Wind)
         CALL FD_Terminate(     ErrStat )

      CASE (HAWC_Wind)
         CALL HW_Terminate(     ErrStat )

      CASE ( Undef_Wind )
         ! Do nothing

      CASE DEFAULT  ! keep this check to make sure that all new wind types have a terminate function
         CALL WrScr(' InflowWind: Undefined wind type in IfW_End().' )
         ErrStat = 1

   END SELECT

!   IF (CT_Flag) CALL CT_Terminate( ErrStat )
   CALL CT_Terminate( ErrStat )


      ! Reset the wind type so that the initialization routine must be called
   ParamData%WindFileType = Undef_Wind
!FIXME: reset the initialization flag.
   ParamData%CT_Flag  = .FALSE.


END SUBROUTINE IfW_End
!====================================================================================================
END MODULE InflowWind



