!**********************************************************************************************************************************
! $Id$
!
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
!    Feb 2013    v2.00.00a-adp   conversion to Framework       A. Platt
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
MODULE InflowWind_Module


   USE                              SharedInflowDefs
!   USE                              InflowWind_Types   !FIXME: this file will replace SharedInflowDefs when I can get it to work with the framework registry generator.
   USE                              NWTC_Library
   USE                              InflowWind_Module_Types

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



      ! ..... Public Subroutines ...................................................................................................

   PUBLIC :: IfW_Init                                 ! Initialization routine
   PUBLIC :: IfW_CalcOutput                           ! Calculate the wind velocities
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
   INTEGER                                   :: UnWind          ! The unit number used for wind inflow files


   !-------------------------------------------------------------------------------------------------
   ! Definitions of public types and routines
   !-------------------------------------------------------------------------------------------------

!FIXME: should not be public anymore
!   PUBLIC                         :: InflowWind_GetVelocity    ! function to get wind speed at point in space and time

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
SUBROUTINE IfW_Init( InitData, InputGuess, ParamData, ContStates, DiscStates, ConstrStateGuess, OtherStates, &
                     OutData, Interval, ErrStat, ErrMsg )
! This routine is called at the start of the simulation to perform initialization steps.
! The parameters are set here and not changed during the simulation.
! The initial states and initial guess for the input are defined.
!----------------------------------------------------------------------------------------------------
!  Open and read the wind files, allocating space for necessary variables

      USE CTWind

         ! Initialization data and guesses

      TYPE( IfW_InitInputType ),          INTENT(IN   )  :: InitData          ! Input data for initialization
      TYPE( IfW_InputType ),              INTENT(  OUT)  :: InputGuess        ! An initial guess for the input; the input mesh must be defined
      TYPE( Ifw_ParameterType ),          INTENT(  OUT)  :: ParamData         ! Parameters
      TYPE( IfW_ContinuousStateType ),    INTENT(  OUT)  :: ContStates        ! Initial continuous states
      TYPE( IfW_DiscreteStateType ),      INTENT(  OUT)  :: DiscStates        ! Initial discrete states
      TYPE( IfW_ConstraintStateType ),    INTENT(  OUT)  :: ConstrStateGuess  ! Initial guess of the constraint states
      TYPE( IfW_OtherStateType ),         INTENT(  OUT)  :: OtherStates       ! Initial other/optimization states
      TYPE( IfW_OutputType ),             INTENT(  OUT)  :: OutData           ! Initial output (outputs are not calculated; only the output mesh is initialized)
      REAL(DbKi),                         INTENT(INOUT)  :: Interval          ! Coupling interval in seconds: InflowWind does not change this.


         ! Error Handling

      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat           ! Error status of the operation
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg            ! Error message if ErrStat /= ErrID_None


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

         CALL CT_Init(UnWind, ParamData%WindFileName, BackGrndValues, ErrStat, ErrMsg)
         IF (ErrStat /= 0) THEN
   !         CALL IfW_End( ParamData, ErrStat )
   !FIXME: cannot call IfW_End here -- requires InitData to be INOUT. Not allowed by framework.
   !         CALL IfW_End( InitData, ParamData, ContStates, DiscStates, ConstrStateGuess, OtherStates, &
   !                       OutData, ErrStat, ErrMsg )
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
            IF (ErrStat == ErrID_None .AND. ParamData%CT_Flag) &
               CALL CT_SetRefVal(InitData%ReferenceHeight, REAL(0.0, ReKi), ErrStat, ErrMsg)


         CASE (FF_Wind)

            CALL FF_Init( UnWind, ParamData%WindFileName, ErrStat, ErrMsg )


               ! Set CT parameters
            IF ( ErrStat == ErrID_None .AND. ParamData%CT_Flag ) THEN
               Height     = FF_GetValue('HubHeight', ErrStat, ErrMsg)
               IF ( ErrStat /= 0 ) Height = InitData%ReferenceHeight

               HalfWidth  = 0.5*FF_GetValue('GridWidth', ErrStat, ErrMsg)
               IF ( ErrStat /= 0 ) HalfWidth = 0

               CALL CT_SetRefVal(Height, HalfWidth, ErrStat, ErrMsg)
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


         ! check error status. If the error is recoverable (ErrID_Warn or less), set flag to indicate we are initialized.

      IF ( ErrStat >= ErrID_Severe ) THEN
         ParamData%Initialized = .FALSE.
         ParamData%WindFileType    = Undef_Wind

            ! Just in case something was allocated
         CALL IfW_End( InputGuess, ParamData, ContStates, DiscStates, ConstrStateGuess, OtherStates, &
                       OutData, ErrStat, ErrMsg )
      ELSE
         ParamData%Initialized = .TRUE.
      END IF

      RETURN

 END SUBROUTINE IfW_Init
 !====================================================================================================
 SUBROUTINE IfW_CalcOutput( Time, InputData, ParamData, &
                              ContStates, DiscStates, ConstrStates, OtherStates, &   ! States -- none in this case
                              OutputData, ErrStat, ErrMsg )
   ! This routine takes an input dataset of type InputType which contains a position array of dimensions 3*n. It then calculates
   ! and returns the output dataset of type OutputType which contains a corresponding velocity array of dimensions 3*n. The input

   ! array contains XYZ triplets for each position of interest (first index is X/Y/Z for values 1/2/3, second index is the point
   ! number to evaluate). The returned values in the OutputData are similar with U/V/W for the first index of 1/2/3.
   !----------------------------------------------------------------------------------------------------

         ! Inputs / Outputs

      REAL( DbKi ),                       INTENT(IN   )  :: Time              ! Current simulation time in seconds
      TYPE( IfW_InputType ),              INTENT(IN   )  :: InputData         ! Inputs at Time
      TYPE( Ifw_ParameterType ),          INTENT(IN   )  :: ParamData         ! Parameters
      TYPE( IfW_ContinuousStateType ),    INTENT(IN   )  :: ContStates        ! Continuous states at Time
      TYPE( IfW_DiscreteStateType ),      INTENT(IN   )  :: DiscStates        ! Discrete states at Time
      TYPE( IfW_ConstraintStateType ),    INTENT(IN   )  :: ConstrStates      ! Constraint states at Time
      TYPE( IfW_OtherStateType ),         INTENT(INOUT)  :: OtherStates       ! Other/optimization states at Time
      TYPE( IfW_OutputType ),             INTENT(INOUT)  :: OutputData        ! Outputs computed at Time (IN for mesh reasons -- not used here)

      INTEGER( IntKi ),                   INTENT(  OUT)  :: ErrStat           ! Error status of the operation
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg            ! Error message if ErrStat /= ErrID_None


         ! Local variables
      REAL( ReKi )                                       :: Position(3)       ! Current position XYZ coords
      INTEGER( IntKi )                                   :: PointCounter      ! loop counter


         ! Sub modules use the InflIntrpOut derived type to store the wind information
!      TYPE(InflIntrpOut)                                 :: CTWindSpeed       ! U, V, W velocities to superimpose on background wind
!      TYPE(InflIntrpOut)                                 :: TempWindSpeed     ! U, V, W velocities returned
      REAL(ReKi)                                         :: CTWindSpeed(3)     ! U, V, W velocities to superimpose on background wind
      REAL(ReKi)                                         :: TempWindSpeed(3)   ! Temporary U, V, W velocities




         ! Initialize ErrStat
      ErrStat  = ErrID_None
      ErrMsg   = ""

         ! Check and see if the output Velocity array has been allocated. If it has, make sure it is the same size as the input position array.
      IF (.not. ALLOCATED(InputData%Position)) THEN
         ErrMsg   = 'Ifw_CalcOutput: The InputData%Position array has not been allocated.'
         ErrStat  = ErrID_Fatal
         RETURN
      ENDIF
      IF (ALLOCATED(OutputData%Velocity)) THEN
         IF ( SIZE(InputData%Position, 2) /= SIZE(OutputData%Velocity, 2) ) THEN
            ErrMsg   = 'IfW_CalcOutput: The InputData%Position and OutputData%Velocity arrays are different sizes'
            ErrStat  = ErrID_Fatal
            RETURN
         ENDIF
      ELSE
         ErrMsg   = 'IfW_CalcOutput: The OutputData%Velocity array has not been allocated.'
         ErrStat  = ErrID_Fatal
         RETURN
      ENDIF



         ! Compute the wind velocities by stepping through all the data points and calling the appropriate GetWindSpeed routine
      SELECT CASE ( ParamData%WindFileType )
         CASE (HH_Wind)

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               OutputData%Velocity(:,PointCounter) = HH_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat, ErrMsg)

                  ! Error Handling -- move ErrMsg inside HH_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in HH_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF
            ENDDO


         CASE (FF_Wind)

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               OutputData%Velocity(:,PointCounter) = FF_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat, ErrMsg)

                  ! Error Handling -- move ErrMsg inside FF_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in FF_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF
            ENDDO


         CASE (UD_Wind)

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               OutputData%Velocity(:,PointCounter) = UsrWnd_GetWindSpeed( Time, InputData%Position(:,PointCounter), ErrStat )!, ErrMsg)

                  ! Error Handling -- move ErrMsg inside UsrWind_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in UsrWnd_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF
            ENDDO


         CASE (FD_Wind)

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               OutputData%Velocity(:,PointCounter) = FD_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat )

                  ! Error Handling -- move ErrMsg inside FD_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in FD_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF
            ENDDO


         CASE (HAWC_Wind)

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               OutputData%Velocity(:,PointCounter) = HW_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat )

                  ! Error Handling -- move ErrMsg inside HW_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in HW_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF
            ENDDO



            ! If it isn't one of the above cases, we have a problem and won't be able to continue

         CASE DEFAULT

            ErrMsg = ' Error: Undefined wind type in IfW_CalcOutput. ' &
                      //'Call WindInflow_Init() before calling this function.'
            ErrStat = ErrID_Fatal               ! No data returned
            OutputData%Velocity(:,:) = 0.0
            RETURN

      END SELECT


         ! If we had a severe or fatal error, we need to make sure we zero out the result and return.

      IF (ErrStat >= ErrID_Severe) THEN

         OutputData%Velocity(:,:) = 0.0
         RETURN

      ELSE

            ! Add coherent turbulence to background wind

         IF (ParamData%CT_Flag) THEN

            DO PointCounter = 1, SIZE(InputData%Position, 2)

               TempWindSpeed = CT_GetWindSpeed(     Time, InputData%Position(:,PointCounter), ErrStat, ErrMsg )

                  ! Error Handling -- move ErrMsg inside CT_GetWindSPeed and simplify
               IF (ErrStat >= ErrID_Severe) THEN
                  ErrMsg   = 'IfW_CalcOutput: Error in CT_GetWindSpeed for point number '//TRIM(Num2LStr(PointCounter))
                  EXIT        ! Exit the loop
               ENDIF

               OutputData%Velocity(:,PointCounter) = OutputData%Velocity(:,PointCounter) + TempWindSpeed

            ENDDO

               ! If something went badly wrong, Return
            IF (ErrStat >= ErrID_Severe ) RETURN

         ENDIF

      ENDIF



END SUBROUTINE IfW_CalcOutput

!====================================================================================================
SUBROUTINE IfW_End( InitData, ParamData, ContStates, DiscStates, ConstrStateGuess, OtherStates, &
                       OutData, ErrStat, ErrMsg )
   ! Clean up the allocated variables and close all open files.  Reset the initialization flag so
   ! that we have to reinitialize before calling the routines again.
   !----------------------------------------------------------------------------------------------------
      USE InflowWind_Module_Types

         ! Initialization data and guesses

      TYPE( IfW_InputType ),              INTENT(INOUT)  :: InitData          ! Input data for initialization
      TYPE( Ifw_ParameterType ),          INTENT(INOUT)  :: ParamData         ! Parameters
      TYPE( IfW_ContinuousStateType ),    INTENT(INOUT)  :: ContStates        ! Continuous states
      TYPE( IfW_DiscreteStateType ),      INTENT(INOUT)  :: DiscStates        ! Discrete states
      TYPE( IfW_ConstraintStateType ),    INTENT(INOUT)  :: ConstrStateGuess  ! Guess of the constraint states
      TYPE( IfW_OtherStateType ),         INTENT(INOUT)  :: OtherStates       ! Other/optimization states
      TYPE( IfW_OutputType ),             INTENT(INOUT)  :: OutData           ! Output data


         ! Error Handling

      INTEGER( IntKi ),                   INTENT(  OUT)  :: ErrStat
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg


         ! Close the wind file, if it happens to be open

      CLOSE( UnWind )


         ! End the sub-modules (deallocates their arrays and closes their files):

      SELECT CASE ( ParamData%WindFileType )

         CASE (HH_Wind)
            CALL HH_Terminate(     ErrStat )

         CASE (FF_Wind)
            CALL FF_Terminate(     ErrStat, ErrMsg )

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

  !   IF (CT_Flag) CALL CT_Terminate( ErrStat ) !FIXME: should it be this line or the next?
         CALL CT_Terminate( ErrStat, ErrMsg )


         ! Reset the wind type so that the initialization routine must be called
      ParamData%WindFileType = Undef_Wind
      ParamData%Initialized = .FALSE.
!FIXME: reset the initialization flag.
      ParamData%CT_Flag  = .FALSE.


END SUBROUTINE IfW_End
!====================================================================================================
END MODULE InflowWind_Module

!SUBROUTINE IfW_HHWind_Init(InitData,   InputGuess, ParamData,                       &
!                           ContStates, DiscStates, ConstrStates,     OtherStates,   &
!                           OutData,    Interval,   ErrStat,          ErrMsg)
!SUBROUTINE IfW_HHWind_CalcOutput(Time,    InData,        ParamData,                       &
!                           ContStates,    DiscStates,    ConstrStates,     OtherStates,   &
!                           OutData,       ErrStat,       ErrMsg)
!SUBROUTINE IfW_HHWind_End( InData,     ParamData,                                &
!                           ContStates, DiscStates, ConstrStates,  OtherStates,   &
!                           OutData,                                              &
!                           ErrStat,    ErrMsg)
!SUBROUTINE IfW_HHWind_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!SUBROUTINE IfW_HHWind_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
!SUBROUTINE IfW_HHWind_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!SUBROUTINE IfW_HHWind_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
