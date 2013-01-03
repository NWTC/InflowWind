!****************************************************************************
!
!  PROGRAM: InflowWind_Test  - This program tests the inflow wind module
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

PROGRAM InflowWind_Test

   USE InflowWind_Module
   USE WindFile_Types
   USE SharedInflowDefs

   IMPLICIT NONE


   REAL(ReKi)          :: InpPosition(3)
   TYPE(InflIntrpOut)  :: MyWindSpeed
   REAL(ReKi)          :: Time

   REAL(DbKi)          :: dt
   INTEGER             :: I

      ! Error Handling

   INTEGER(IntKi)                                     :: ErrStat
   CHARACTER(1024)                                    :: ErrMsg


      ! All the shared types used in the module
   TYPE( IfW_ParameterType )                          :: IfW_ParamData     ! Parameters
   TYPE( IfW_InitInputType )                          :: IfW_InitData      ! data for initialization


   !-------------------------------------------------------------------------------------------------
   ! Send the data required for initialization
   !-------------------------------------------------------------------------------------------------

!      IfW_InitData%WindFileName     = "D:\DATA\Fortran\IVF Projects\AeroDyn\Update\Source\InflowWind\TestData\GPLLJ_DNS\InOut.wnd"
!      IfW_InitData%WindFileName     = "../TestRoutines/TestData/Periodic_Winds.wnd"    !! ff wind
!      IfW_InitData%WindFileName     = "Test-Data/InOut.wnd"    !! ff wind
      IfW_InitData%WindFileName     = "../Samples/Steady.wnd"  !! HH wind
!      IfW_InitData%WindFileName     = "../Samples/les.fdp"  !! 4 D -- points to some other files. -- not work
      IfW_InitData%ReferenceHeight  = 80.   ! meters
      IfW_InitData%Width            = 100.  ! meters

!     IfW_InitData%WindFileType     = FF_Wind
      IfW_InitData%WindFileType     = DEFAULT_Wind      ! let the module figure out what type of file it is...


      CALL IfW_Init( IfW_InitData, IfW_ParamData, dt, ErrStat, ErrMsg )


      IF (errstat /=0) CALL ProgAbort('Error in Initialization routine')


   !-------------------------------------------------------------------------------------------------
   ! Get the wind speeds at various times and positions
   !-------------------------------------------------------------------------------------------------
      dt     = 0.05 ! seconds

      InpPosition(1) = 0.0                            ! longitudinal position front/back of tower
      InpPosition(2) = 0.0                            ! lateral position left/right of tower
      InpPosition(3) = IfW_InitData%ReferenceHeight   ! height relative to the ground

      DO I = 1,3 !time

         Time = 0.0 + (I-1)*dt

         MyWindSpeed = InflowWind_GetVelocity( IfW_ParamData, Time, InpPosition, ErrStat )

         !IF (ErrStat /=0) CALL ProgAbort('Error in getting wind speed')

         WRITE(*,*) TRIM(Num2LStr(ErrStat)), ' V(t=', TRIM(Num2LStr(Time)), ') = ', MyWindSpeed

      END DO

   !-------------------------------------------------------------------------------------------------
   ! Clean up the variables and close files
   !-------------------------------------------------------------------------------------------------
    CALL IfW_End( IfW_ParamData, ErrStat )


END PROGRAM InflowWind_Test

