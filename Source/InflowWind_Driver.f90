!****************************************************************************
!
!  PROGRAM: InflowWind_Driver  - This program tests the inflow wind module
!
!****************************************************************************
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

PROGRAM InflowWind_Driver

!   USE NWTC_Library       !NOTE: Not sure why this doesn't need to be specified
   USE InflowWind_Module
   USE WindFile_Types      !NOTE: would we prefer not to need this????
   USE SharedInflowDefs
   USE IfW_Driver_Types    ! Contains types and routines for handling the input arguments
   USE IfW_Driver_Subs     ! Contains subroutines for the driver program

   IMPLICIT NONE

      ! Info on this code
   TYPE( ProgDesc ), PARAMETER                        :: ProgInfo = ProgDesc("InflowWind_Driver","v1.00.00a-adp","31-Dec-2012")


      ! Types needed here (from InflowWind module)
   TYPE( IfW_InitInputType )                          :: IfW_InitInputData       ! Data for initialization -- this is where the input info goes
   TYPE( IfW_InputType )                              :: IfW_InputData           ! input     -- contains xyz coords of interest
   TYPE( IfW_ParameterType )                          :: IfW_ParamData           ! Parameters
   TYPE( IfW_ContinuousStateType )                    :: IfW_ContStateData       ! Continous State Data  (not used here)
   TYPE( IfW_DiscreteStateType )                      :: IfW_DiscStateData       ! Discrete State Data   (not used here)
   TYPE( IfW_ConstraintStateType )                    :: IfW_ConstrStateData     ! Constraint State Data (not used here)
   TYPE( IfW_OtherStateType )                         :: IfW_OtherStateData      ! Other State Data      (might use at some point)
   TYPE( IfW_OutputType )                             :: IfW_OutputData          ! Output Data -- contains the velocities at xyz

      ! Error Handling
   INTEGER(IntKi)                                     :: ErrStat
   CHARACTER(1024)                                    :: ErrMsg



      ! Local variables for this code
   TYPE( IfW_Driver_ArgFlags )                        :: SettingsFlags        ! Flags indicating which arguments were specified
   TYPE( IfW_Driver_Args )                            :: Settings             ! Arguments passed in
   REAL( DbKi )                                       :: TimeStep             ! Initial timestep (the glue code ditcates this)
   REAL( DbKi )                                       :: Timer(1:2)           ! Keep track of how long this takes to run
   INTEGER( IntKi )                                   :: InputArgs            ! Number of arguments passed in
   LOGICAL                                            :: TempFileExist        ! Flag for inquiring file existence



   !--------------------------------------------------------------------------
   !-=-=- Initialize the Library -=-=-
   !--------------------------------------------------------------------------

   CALL NWTC_Init
   CALL DispNVD(ProgInfo)


!FIXME
      ! Set the beep to false. This is a temporary workaround since the beepcode on linux may be incorrectly set. This may also be an artifact of my current development environment.
   Beep = .FALSE.


   !--------------------------------------------------------------------------
   !-=-=- Setup the program -=-=-
   !--------------------------------------------------------------------------

      ! Start the timer
   CALL CPU_TIME( Timer(1) )



   !--------------------------------------------------------------------------
   !-=-=- Parse the command line inputs -=-=-
   !--------------------------------------------------------------------------

   CALL RetrieveArgs( Settings, SettingsFlags, ErrStat, ErrMsg )

   IF ( ErrStat == ErrID_Fatal ) CALL ProgAbort( ErrMsg )


      ! Set the input file name and verify it exists

   IfW_InitInputData%WindFileName      = Settings%InputFile

   INQUIRE( file=Settings%InputFile, exist=TempFileExist )
   IF ( TempFileExist .eqv. .FALSE. ) CALL ProgAbort( "Cannot find input file "//TRIM(Settings%InputFile))


      ! In the event things are not specified on the input line, use the following

   IfW_InitInputData%ReferenceHeight   = 80.                      ! meters  -- default
   IfW_InitInputData%Width             = 100.                     ! meters
   IfW_InitInputData%WindFileType      = DEFAULT_Wind             ! This must be preset before calling the initialization.
   TimeStep                            = 10                       !seconds


      ! If they are specified by input arguments, use the following

   IF ( SettingsFlags%Height )         IfW_InitInputData%ReferenceHeight = Settings%Height
   IF ( SettingsFlags%Width )          IfW_InitInputData%Width           = Settings%Width
   IF ( SettingsFlags%WindFileType )   IfW_InitInputData%WindFileType    = Settings%WindFileType
   IF ( SettingsFlags%Tres )           TimeStep                          = Settings%Tres



      ! Sanity check: if an input points file is specified, make sure it actually exists.

   IF ( SettingsFlags%PointsFile ) THEN
      INQUIRE( file=Settings%PointsFile, exist=TempFileExist )
      IF ( TempFileExist .eqv. .FALSE. ) CALL ProgAbort( "Cannot find the points file "//TRIM(Settings%InputFile))
   ENDIF



   !--------------------------------------------------------------------------
   !-=-=- Initialize the Module -=-=-
   !--------------------------------------------------------------------------
   !  Initialize the IfW module --> it will initialize all its pieces

   CALL IfW_Init( IfW_InitInputData, IfW_InputData, IfW_ParamData, &
                  IfW_ContStateData, IfW_DiscStateData, IfW_ConstrStateData, IfW_OtherStateData, &
                  IfW_OutputData, TimeStep, ErrStat, ErrMsg )


      ! Make sure no errors occured that give us reason to terminate now.

   IF ( ErrStat == ErrID_Severe ) CALL ProgAbort( ErrMsg )
   IF ( ErrStat == ErrID_Fatal )  CALL ProgAbort( ErrMsg )



   !--------------------------------------------------------------------------
   !-=-=- Other Setup -=-=-
   !--------------------------------------------------------------------------
   !  Setup any additional things
   !  -- reset bounds to reasonable level (can't do more than what actually exists in the file)
   !  -- setup the matrices for handling the data?





   !FIXME: check that the FFT file can be made


   !--------------------------------------------------------------------------
   !-=-=- Time stepping loop -=-=-
   !--------------------------------------------------------------------------
   !  Loop through the time
   !     -- send matrix of coordinates at each timestep --> ask for certain points at time T
   !     -- should get back a matrix of wind velocities at each coordinate
   !     -- Assemble the large matrix with all the small pieces

!FIXME: write this routine
!   CALL IfW_Calculate( )


   !--------------------------------------------------------------------------
   !-=-=- Calculate OtherStates -=-=-
   !--------------------------------------------------------------------------
   !  Iff we add in some averaging / TI / mean etc, it would be in OtherStates
   !     -- Test that here.




   !--------------------------------------------------------------------------
   !-=-=- Output results -=-=-
   !--------------------------------------------------------------------------
   !  ParaPrint         -- will create a ParaView file that can be looked at
   !  Write to screen   -- if ParaPrint isn't used


   !--------------------------------------------------------------------------
   !-=-=- We are done, so close everything down -=-=-
   !--------------------------------------------------------------------------

   CALL IfW_End(  IfW_InitInputData, IfW_ParamData, &
                  IfW_ContStateData, IfW_DiscStateData, IfW_ConstrStateData, IfW_OtherStateData, &
                  IfW_OutputData, ErrStat, ErrMsg )








END PROGRAM InflowWind_Driver




