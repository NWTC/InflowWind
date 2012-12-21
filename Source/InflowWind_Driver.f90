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
   USE WindFile_Types     !NOTE: would we prefer not to need this????
   USE SharedInflowDefs

   IMPLICIT NONE


      ! Parametertypes needed
   TYPE( IfW_InitInputType )                          :: IfW_InitInputData       ! Data for initialization -- this is where the input info goes
   TYPE( IfW_InputType )                              :: IfW_InputData           ! input     -- contains xyz coords of interest
!   TYPE( IfW_InputType )                              :: IfW_InputGuessData      ! initial guess for the input (not needed here -- no initial conditions)
   TYPE( IfW_ParameterType )                          :: IfW_ParamData           ! Parameters
   TYPE( IfW_ContinuousStateType )                    :: IfW_ContStateData       ! Continous State Data  (not used here)
   TYPE( IfW_DiscreteStateType )                      :: IfW_DiscStateData       ! Discrete State Data   (not used here)
   TYPE( IfW_ConstraintStateType )                    :: IfW_ConstrStateData     ! Constraint State Data (not used here)
   TYPE( IfW_OtherStateType )                         :: IfW_OtherStateData      ! Other State Data      (might use at some point)
   TYPE( IfW_OutputType )                             :: IfW_OutputData          ! Output Data -- contains the velocities at xyz

      ! Setup some variables for this code
   REAL( DbKi )                                       :: TimeStep             ! Initial timestep (the glue code ditcates this)
   REAL( DbKi )                                       :: Timer(1:2)           ! Keep track of how long this takes to run

      ! Error Handling
   INTEGER(IntKi)                                     :: ErrStat
   CHARACTER(1024)                                    :: ErrMsg



   !--------------------------------------------------------------------------
   !-=-=- Setup the program -=-=-
   !--------------------------------------------------------------------------

      ! Start the timer
   CALL CPU_TIME( Timer(1) )

      ! Temporary assign some things for now. This will be taken from the command line later
   IfW_InitInputData%WindFileName      = "../Samples/Steady.wnd"  ! HHWind file

   IfW_InitInputData%ReferenceHeight   = 80.                      ! meters
   IfW_InitInputData%Width             = 100.                     ! meters

   IfW_InitInputData%WindFileType      = DEFAULT_Wind             ! This must be preset before calling the initialization.

   TimeStep                            = 0.5                      !seconds


   !--------------------------------------------------------------------------
   !-=-=- Parse the command line inputs etc -=-=-
   !--------------------------------------------------------------------------
   !  -- Grab the input filename
   !  -- get bounds on time / timestep
   !  -- get bounds on space coordinates from input / resolution
   !     -- may want to be able to do just the turbine plane region
   !     -- may want to be able to do the entire region before (maybe after -- but the turbine changes what that looks like)



   !--------------------------------------------------------------------------
   !-=-=- Initialize the Module -=-=-
   !--------------------------------------------------------------------------
   !  Initialized the IfW module --> it should initialize all submodules

!   CALL IfW_Init( IfW_InitInputData, IfW_ParamData, TimeStep, ErrStat, ErrMsg )
   CALL IfW_Init( IfW_InitInputData, IfW_InputData, IfW_ParamData, &                                  ! Inputs and parameters
                  IfW_ContStateData, IfW_DiscStateData, IfW_ConstrStateData, IfW_OtherStateData, &    ! States FIXME: might be ConstrStateGuess!!!!
                  IfW_OutputData, TimeStep, ErrStat, ErrMsg )

      ! Some simple error checking.
   IF (errStat /=0) CALL ProgAbort( 'Error occured during the initialization routine' )



   !--------------------------------------------------------------------------
   !-=-=- Other Setup -=-=-
   !--------------------------------------------------------------------------
   !  Setup any additional things
   !  -- reset bounds to reasonable level (can't do more than
   !  -- setup the matrices for handling the data?




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

   CALL IfW_End( IfW_ParamData, ErrStat )








END PROGRAM InflowWind_Driver

