!**********************************************************************************************************************************
!
!  MODULE: IfW_Driver_Types  - This module contains types used by the InflowWind Driver program to store arguments passed in
!
!  The types listed here are used within the InflowWind Driver program to store the settings. These settings are read in as
!  command line arguments, then stored within these types.
!
!**********************************************************************************************************************************
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

MODULE Ifw_Driver_Types

   USE NWTC_Library
   USE WindFile_Types

   IMPLICIT NONE

      ! This contains flags to note if the settings were made.
   TYPE     :: IfW_Driver_ArgFlags
      LOGICAL                 :: WindFileType   = .FALSE.      ! specified a windfiletype
      LOGICAL                 :: Height         = .FALSE.      ! specified a height
      LOGICAL                 :: Width          = .FALSE.      ! specified a width
      LOGICAL                 :: Xrange         = .FALSE.      ! specified a range of x
      LOGICAL                 :: Yrange         = .FALSE.      ! specified a range of y
      LOGICAL                 :: Zrange         = .FALSE.      ! specified a range of z
      LOGICAL                 :: Trange         = .FALSE.      ! specified a range of time
      LOGICAL                 :: Xres           = .FALSE.      ! specified a resolution in x
      LOGICAL                 :: Yres           = .FALSE.      ! speficied a resolution in y
      LOGICAL                 :: Zres           = .FALSE.      ! specified a resolution in z
      LOGICAL                 :: Tres           = .FALSE.      ! specified a resolution in time
      LOGICAL                 :: ParaPrint      = .FALSE.      ! create a ParaView file?
      LOGICAL                 :: Summary        = .FALSE.      ! create a summary file?
      LOGICAL                 :: fft            = .FALSE.      ! do an FFT
      LOGICAL                 :: PointsFile     = .FALSE.      ! points file specified
   END TYPE    IfW_Driver_ArgFlags


      ! This contains all the settings (possible passed in arguments).
   TYPE     :: IfW_Driver_Args
      INTEGER                 :: WindFileType   = DEFAULT_WIND ! the kind of windfile     -- set default to simplify things later
      REAL( ReKi )            :: Height                        ! Reference height
      REAL( ReKi )            :: Width                         ! Reference width
      REAL( ReKi )            :: Xrange(1:2)                   ! range of x
      REAL( ReKi )            :: Yrange(1:2)                   ! range of y
      REAL( ReKi )            :: Zrange(1:2)                   ! range of z
      REAL( ReKi )            :: Trange(1:2)                   ! range of time
      REAL( ReKi )            :: Xres                          ! resolution of x
      REAL( ReKi )            :: Yres                          ! resolution of y
      REAL( ReKi )            :: Zres                          ! resolution of z
      REAL( ReKi )            :: Tres                          ! resolution of time
      REAL( ReKi )            :: fft(1:3)                      ! Coords to do an FFT
      CHARACTER(1024)         :: PointsFile                    ! Filename of points file
      CHARACTER(1024)         :: InputFile                     ! Filename of file to process
   END TYPE    IfW_Driver_Args


END MODULE IfW_Driver_Types
