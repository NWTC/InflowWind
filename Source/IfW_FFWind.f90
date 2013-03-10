MODULE IfW_FFWind
!
!  This module uses full-field binary wind files to determine the wind inflow.
!  This module assumes that the origin, (0,0,0), is located at the tower centerline at ground level,
!  and that all units are specified in the metric system (using meters and seconds).
!  Data is shifted by half the grid width to account for turbine yaw (so that data in the X
!  direction actually starts at -1*FFYHWid meters).
!
!  Created 25-Sept-2009 by B. Jonkman, National Renewable Energy Laboratory
!     using subroutines and modules from AeroDyn v12.58
!
!----------------------------------------------------------------------------------------------------
!  Feb 2013    v2.00.00          A. Platt    -- updated to the new framework
!                    - Note:  Jacobians are not included in this version.
!
!----------------------------------------------------------------------------------------------------
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
!----------------------------------------------------------------------------------------------------

   USE                                       NWTC_Library
   USE                                       SharedInflowDefs
   USE                                       IfW_FFWind_Types

   IMPLICIT                                  NONE
   PRIVATE

   INTEGER(IntKi),   PARAMETER               :: DataFormatID = 1   ! Update this value if the data types change (used in IfW_FFWind_Pack)
   TYPE(ProgDesc),   PARAMETER               :: IfW_FFWind_ProgDesc = ProgDesc( 'IfW_FFWind', 'v1.00.00', '25-Feb-2013' )

   PUBLIC                                    :: IfW_FFWind_Init
   PUBLIC                                    :: IfW_FFWind_End
!   PUBLIC                                    :: IfW_FFWind_CalcOutput


      ! The following do not contain anything since there are no states.
   PUBLIC                                    :: IfW_FFWind_UpdateStates
   PUBLIC                                    :: IfW_FFWind_CalcContStateDeriv
   PUBLIC                                    :: IfW_FFWind_UpdateDiscState
   PUBLIC                                    :: IfW_FFWind_CalcConstrStateResidual


      !The following were removed during conversion to the framework:

!====================================================================================================


      ! former FF_Wind module

!   REAL(ReKi), ALLOCATABLE          :: FFData  (:,:,:,:)          ! Array of FF data
!   REAL(ReKi), ALLOCATABLE          :: FFtower (:,:,:)            ! Array of data along the tower, below the FF array

   REAL(ReKi)                       :: FFDTime                    ! delta time
   REAL(ReKi)                       :: FFRate                     ! data rate in Hz (1/FFDTime)
   REAL(ReKi)                       :: FFYHWid                    ! half the grid width
   REAL(ReKi)                       :: FFZHWid                    ! half the grid height
   REAL(ReKi)                       :: RefHt                      ! the reference (hub) height of the grid in meters
   REAL(ReKi)                       :: GridBase                   ! the height of the bottom of the grid in meters
   REAL(ReKi)                       :: InitXPosition              ! the initial x location of the wind file (distance the FF file will be offset)
   REAL(ReKi)                       :: InvFFYD                    ! reciprocal of delta y
   REAL(ReKi)                       :: InvFFZD                    ! reciprocal of delta z
   REAL(ReKi)                       :: InvMFFWS                   ! reciprocal of the mean wind speed (MeanFFWS)
   REAL(ReKi)                       :: MeanFFWS                   ! the mean wind speed (as defined in the FF file), not necessarially the mean of the portion of the wind used
   REAL(ReKi)                       :: TotalTime                  ! the total time in the simulation

   INTEGER(IntKi)                          :: NFFComp                    ! number of wind components
   INTEGER(IntKi)                          :: NFFSteps                   ! number of time steps in the FF array
   INTEGER(IntKi)                          :: NYGrids                    ! number of points in the lateral (y) direction of the grids
   INTEGER(IntKi)                          :: NZGrids                    ! number of points in the vertical (z) direction of the grids
   INTEGER(IntKi)                          :: NTGrids                    ! number of points in the vertical (z) direction on the tower (below the grids)

!FIXME: should not be save -- move to the parameters?
   LOGICAL, SAVE                    :: Initialized = .FALSE.      ! flag that determines if the module has been initialized
!FIXME: invokes SAVE
   LOGICAL                          :: Periodic    = .FALSE.      ! flag that determines if the wind is periodic


!!!   INTERFACE FF_GetValue
!!!      MODULE PROCEDURE FF_GetRValue                               ! routine to return scalar real values
!!!   END INTERFACE


!   PUBLIC                           :: FF_GetWindSpeed            ! interpolation function that returns velocities at specified time and space
!   PUBLIC                           :: FF_GetValue                ! interface to return requested values

CONTAINS
!====================================================================================================
SUBROUTINE IfW_FFWind_Init(InitData,   InputGuess, ParamData,                       &
                           ContStates, DiscStates, ConstrStates,     OtherStates,   &
                           OutData,    Interval,   ErrStat,          ErrMsg)
!  This routine is used read the full-field turbulence data.
!  09/25/97 - Created by M. Buhl from GETFILES in ViewWind.
!  09/23/09 - modified by B. Jonkman: this subroutine was split into several subroutines (was ReadFF)
!----------------------------------------------------------------------------------------------------

   IMPLICIT                       NONE


      ! Passed Variables
   TYPE(IfW_FFWind_InitInputType),        INTENT(IN   )  :: InitData       ! Initialization data passed to the module
   TYPE(IfW_FFWind_InputType),            INTENT(  OUT)  :: InputGuess     ! Initialized input data variable
   TYPE(IfW_FFWind_ParameterType),        INTENT(  OUT)  :: ParamData      ! Parameters
   TYPE(IfW_FFWind_ContinuousStateType),  INTENT(  OUT)  :: ContStates     ! Continuous States  (unused)
   TYPE(IfW_FFWind_DiscreteStateType),    INTENT(  OUT)  :: DiscStates     ! Discrete States    (unused)
   TYPE(IfW_FFWind_ConstraintStateType),  INTENT(  OUT)  :: ConstrStates   ! Constraint States  (unused)
   TYPE(IfW_FFWind_OtherStateType),       INTENT(  OUT)  :: OtherStates    ! Other State data   (storage for the main data)
   TYPE(IfW_FFWind_OutputType),           INTENT(  OUT)  :: OutData        ! Initial output

   REAL(DbKi),                            INTENT(INOUT)  :: Interval       ! Time Interval to use (passed through here)


      ! Error Handling
   INTEGER(IntKi),                        INTENT(  OUT)  :: ErrStat        ! determines if an error has been encountered
   CHARACTER(1024),                       INTENT(  OUT)  :: ErrMsg         ! Message about errors


      ! Temporary variables for error handling
   INTEGER(IntKi)                                        :: TmpErrStat     ! temporary error status
   CHARACTER(1024)                                       :: TmpErrMsg      ! temporary error message


      ! Local Variables:

   REAL(ReKi)                                            :: TI      (3)    ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
   REAL(ReKi)                                            :: BinTI   (3)    ! turbulence intensities of the wind components as defined in the FF binary file, not necessarially the actual TI
   REAL(ReKi)                                            :: UBar
   REAL(ReKi)                                            :: ZCenter

   INTEGER(B2Ki)                                         :: Dum_Int2
   INTEGER(IntKi)                                        :: DumInt
   INTEGER(IntKi)                                        :: I
   LOGICAL                                               :: CWise
   LOGICAL                                               :: Exists
   CHARACTER( 1028 )                                     :: SumFile        ! length is LEN(ParamData%WindFileName) + the 4-character extension.
   CHARACTER( 1028 )                                     :: TwrFile        ! length is LEN(ParamData%WindFileName) + the 4-character extension.



      !-------------------------------------------------------------------------------------------------
      ! Initialize temporary variables
      !-------------------------------------------------------------------------------------------------

   ErrMsg   = ''
   ErrStat  = ErrID_None

   TmpErrMsg   = ''
   TmpErrStat  = ErrID_None


      !-------------------------------------------------------------------------------------------------
      ! Check that it's not already initialized
      !-------------------------------------------------------------------------------------------------

   IF ( ParamData%Initialized ) THEN
      ErrMsg   = ' FFWind has already been initialized.'
      ErrStat  = ErrId_Warn      ! no reason to stop the program over this
      RETURN
   ELSE
      ErrStat = ErrId_None

   ENDIF


      ! Get a unit number to use

   CALL GetNewUnit(OtherStates%UnitWind, TmpErrStat, TmpErrMsg)
   IF ( TmpErrStat /= 0 ) THEN
      ErrStat  = ErrID_Fatal
      ErrMsg   = TRIM(ErrMsg)//NewLine//"IfW_FFWind: Could not assign a unitnumber for opening the Wind file."
      RETURN
   ENDIF


      !-------------------------------------------------------------------------------------------------
      ! Copy things from the InitData to the ParamData
      !-------------------------------------------------------------------------------------------------

   ParamData%ReferenceHeight  =  InitData%ReferenceHeight
   ParamData%Width            =  InitData%Width
   ParamData%WindFileName     =  InitData%WindFileName


      !----------------------------------------------------------------------------------------------
      ! Open the binary file, read its "header" (first 2-byte integer) to determine what format
      ! binary file it is, and close it.
      !----------------------------------------------------------------------------------------------

   CALL OpenBInpFile (OtherStates%UnitWind, TRIM(ParamData%WindFileName), TmpErrStat, TmpErrMsg)
   IF ( TmpErrStat >= AbortErrLev ) THEN
      ErrStat  = MAX(TmpErrStat, ErrStat)
      ErrMsg   = TRIM(ErrMsg)//NewLine//TRIM(TmpErrMsg)
      RETURN
   ENDIF

!FIXME: convert READ to the library ones
   READ ( OtherStates%UnitWind, IOSTAT=TmpErrStat )  Dum_Int2
   CLOSE( OtherStates%UnitWind )

   IF (TmpErrStat /= 0) THEN
      ErrMsg   = ' Error reading first binary integer from file "'//TRIM(ParamData%WindFileName)//'."'
      ErrStat  = ErrID_Fatal
      RETURN
   ENDIF

      !----------------------------------------------------------------------------------------------
      ! Read the files to get the required FF data.
      !----------------------------------------------------------------------------------------------
   DumInt = Dum_Int2  ! change to default INTEGER, instead of INT(2) to compare in SELECT below

   SELECT CASE (DumInt)

      CASE ( 7, 8 )                                                    ! TurbSim binary format

         CALL Read_TurbSim_FF(OtherStates%UnitWind, TRIM(ParamData%WindFileName), TmpErrStat, TmpErrMsg)

         IF ( TmpErrStat >= AbortErrLev ) THEN
            ErrStat  = MAX(TmpErrStat, ErrStat)
            ErrMsg   = TRIM(ErrMsg)//NewLine//TRIM(TmpErrMsg)
            RETURN
         ENDIF

      CASE ( -1, -2, -3, -99 )                                         ! Bladed-style binary format

         !...........................................................................................
         ! Create full-field summary file name from binary file root name.  Also get tower file
         ! name.
         !...........................................................................................

            CALL GetRoot(ParamData%WindFileName, SumFile)

            TwrFile = TRIM(SumFile)//'.twr'
            SumFile = TRIM(SumFile)//'.sum'

         !...........................................................................................
         ! Read the summary file to get necessary scaling information
         !...........................................................................................

            CALL Read_Summary_FF (OtherStates%UnitWind, TRIM(SumFile), CWise, ZCenter, TI, ErrStat, ErrMsg )
            IF ( TmpErrStat >= AbortErrLev ) THEN
               ErrStat  = MAX(TmpErrStat, ErrStat)
               ErrMsg   = TRIM(ErrMsg)//NewLine//TRIM(TmpErrMsg)
               RETURN
            ENDIF

            UBar = MeanFFWS      ! temporary storage .... this is our only check to see if the summary and binary files "match"

!!!         !...........................................................................................
!!!         ! Open the binary file and read its header
!!!         !...........................................................................................
!!!
!!!            CALL OpenBInpFile (OtherStates%UnitWind, TRIM(ParamData%WindFileName), ErrStat, ErrMsg )
!!!!FIXME: Add ErrMsg when it is available
!!!
!!!            IF (ErrStat >= ErrID_Fatal) RETURN
!!!
!!!            IF ( Dum_Int2 == -99 ) THEN                                       ! Newer-style BLADED format
!!!               CALL Read_Bladed_FF_Header1 (OtherStates%UnitWind, BinTI, ErrStat, ErrMsg)
!!!
!!!                  ! If the TIs are also in the binary file (BinTI > 0),
!!!                  ! use those numbers instead of ones from the summary file
!!!
!!!               DO I =1,NFFComp
!!!                  IF ( BinTI(I) > 0 ) TI(I) = BinTI(I)
!!!               ENDDO
!!!
!!!            ELSE
!!!               CALL Read_Bladed_FF_Header0 (OtherStates%UnitWind, ErrStat, ErrMsg)          ! Older-style BLADED format
!!!            ENDIF
!!!
!!!            IF (ErrStat >= ErrID_Fatal) RETURN
!!!
!!!         !...........................................................................................
!!!         ! Let's see if the summary and binary FF wind files go together before continuing.
!!!         !...........................................................................................
!!!
!!!            IF ( ABS( UBar - MeanFFWS ) > 0.1 )  THEN
!!!               ErrMsg   = ' Error: Incompatible mean hub-height wind speeds in FF wind files. '//&
!!!                           '(Check that the .sum and .wnd files were generated together.)'
!!!               ErrStat  = ErrID_Fatal      ! was '= 1'
!!!               RETURN
!!!            ENDIF
!!!
!!!         !...........................................................................................
!!!         ! Calculate the height of the bottom of the grid
!!!         !...........................................................................................
!!!
!!!            GridBase = ZCenter - FFZHWid         ! the location, in meters, of the bottom of the grid
!!!
!!!         !...........................................................................................
!!!         ! Read the binary grids (converted to m/s) and close the file
!!!         !...........................................................................................
!!!
!!!            CALL Read_Bladed_Grids( OtherStates%UnitWind, CWise, TI, ErrStat, ErrMsg)
!!!            CLOSE ( OtherStates%UnitWind )
!!!
!!!            IF ( ErrStat /= 0 ) RETURN
!!!
!!!         !...........................................................................................
!!!         ! Read the tower points file
!!!         !...........................................................................................
!!!
!!!            INQUIRE ( FILE=TRIM(TwrFile) , EXIST=Exists )
!!!
!!!            IF (  Exists )  THEN
!!!               CALL Read_FF_Tower( OtherStates%UnitWind, TRIM(TwrFile), ErrStat, ErrMsg )
!!!            ELSE
!!!               NTgrids = 0
!!!            ENDIF
!!!
!!!
      CASE DEFAULT

         ErrMsg   = TRIM(ErrMsg)//NewLine//' Error: Unrecognized binary wind file type.'
         ErrStat  = ErrID_Fatal
         RETURN

   END SELECT
!!!
!!!
!!!   IF (Periodic) THEN
!!!      InitXPosition = 0                ! start at the hub
!!!      TotalTime     = NFFSteps*FFDTime
!!!   ELSE
!!!      InitXPosition = FFYHWid          ! start half the grid with ahead of the turbine
!!!      TotalTime     = (NFFSteps-1)*FFDTime
!!!   ENDIF
!!!
!!!   ParamData%Initialized = .TRUE.

   RETURN


   CONTAINS

   !====================================================================================================
   SUBROUTINE Read_Summary_FF ( UnitWind, FileName, CWise, ZCenter, TI, ErrStat, ErrMsg )
   ! This subroutine reads the text summary file to get normalizing parameters, the location of the
   ! grid, and the direction the grid was written to the binary file
   !----------------------------------------------------------------------------------------------------

         ! Passed variables
      INTEGER(IntKi),                     INTENT(IN   )  :: UnitWind       ! unit number for the file to open
      CHARACTER(*),                       INTENT(IN   )  :: FileName       ! name of the summary file
      LOGICAL,                            INTENT(  OUT)  :: CWise          ! rotation (for reading the order of the binary data)
      REAL(ReKi),                         INTENT(  OUT)  :: ZCenter        ! the height at the center of the grid
      REAL(ReKi),                         INTENT(  OUT)  :: TI      (3)    ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat        ! returns 0 if no error encountered in the subroutine
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg         ! holds the error messages

        ! Local variables
      REAL(ReKi)                                         :: ZGOffset       ! The vertical offset of the turbine on rectangular grid (allows turbulence not centered on turbine hub)

      INTEGER, PARAMETER                                 :: NumStrings = 6 ! number of strings to be looking for in the file

      INTEGER(IntKi)                                     :: FirstIndx      ! The first character of a line where data is located
      INTEGER(IntKi)                                     :: I              ! A loop counter
      INTEGER(IntKi)                                     :: LastIndx       ! The last  character of a line where data is located
      INTEGER(IntKi)                                     :: LineCount      ! Number of lines that have been read in the file

      LOGICAL                                            :: StrNeeded(NumStrings)   ! if the string has been found

      CHARACTER(1024)                                    :: LINE           ! temporary storage for reading a line from the file

         ! Temporary variables for error handling
      INTEGER(IntKi)                                     :: TmpErrStat     ! temporary error status
      CHARACTER(1024)                                    :: TmpErrMsg      ! temporary error message

         !----------------------------------------------------------------------------------------------
         ! Initialize some variables
         !----------------------------------------------------------------------------------------------

      ErrStat      = ErrID_None
      TmpErrStat   = ErrID_None
      ErrMsg       = ''
      TmpErrMsg    = ''
      LineCount    = 0
      StrNeeded(:) = .TRUE.
      ZGOffset     = 0.0
      RefHt        = 0.0
      Periodic     = .FALSE.

         !----------------------------------------------------------------------------------------------
         ! Open summary file.
         !----------------------------------------------------------------------------------------------

      CALL OpenFInpFile ( OtherStates%UnitWind, TRIM( FileName ), TmpErrStat, TmpErrMsg )
      IF ( TmpErrStat >= AbortErrLev ) THEN
         ErrStat  = MAX(TmpErrStat, ErrStat)
         ErrMsg   = TRIM(ErrMsg)//NewLine//TRIM(TmpErrMsg)
         RETURN
      ENDIF


         !----------------------------------------------------------------------------------------------
         ! Read the summary file.
         !----------------------------------------------------------------------------------------------

      DO WHILE ( ( ErrStat == ErrID_None ) .AND. StrNeeded(NumStrings) )

         LineCount = LineCount + 1

!FIXME: convert READ to the library ones
         READ ( OtherStates%UnitWind, '(A)', IOSTAT=TmpErrStat ) LINE
         IF ( TmpErrStat /= 0 ) THEN

!FIXME: there might be a logic error here......
            IF ( StrNeeded(NumStrings-1) ) THEN  ! the "HEIGHT OFFSET" StrNeeded(NumStrings) parameter is not necessary.  We'll assume it's zero if we didn't find it.
!FIXME: was this fixed? !FIXME: should modify how the ErrMsg is handled. See bug report 438.
               ErrMsg   =  TRIM(ErrMsg)//NewLine//' Error reading line #'//TRIM(Num2LStr(LineCount))//' of the summary file, "'// &
                           TRIM(FileName)//'".'
               ErrMsg   = TRIM(ErrMsg)//' Could not find all of the required parameters.'
               ErrStat  = ErrID_Fatal   !
               RETURN
            ELSE
               EXIT
            ENDIF

         ENDIF

         CALL Conv2UC ( LINE )


         IF ( StrNeeded(1) ) THEN

            !-------------------------------------------------------------------------------------------
            ! #1: Get the rotation direction, using the string "CLOCKWISE"
            !-------------------------------------------------------------------------------------------

            IF ( INDEX( LINE, 'CLOCKWISE' ) > 0 ) THEN

!FIXME: convert READ to the library ones
               READ (LINE, *, IOSTAT = TmpErrStat)  CWise          ! Look for True/False values

               IF ( TmpErrStat /= 0 ) THEN                         ! Look for Yes/No values instead

                  LINE = ADJUSTL ( LINE )                      ! Remove leading spaces from input line

                  SELECT CASE (LINE(1:1) )
                     CASE ('Y')
                        CWise = .TRUE.
                     CASE ('N')
                        CWise = .FALSE.
                     CASE DEFAULT
                        ErrMsg   = TRIM(ErrMsg)//NewLine//' Error reading rotation direction (CLOCKWISE) from FF summary file.'
                        ErrStat  = ErrID_Fatal        ! This is necessary information??
                        RETURN
                  END SELECT

               ENDIF ! TmpErrStat /= 0
               StrNeeded(1) = .FALSE.

            ENDIF   ! INDEX for "CLOCKWISE"

         ELSEIF ( StrNeeded(2) ) THEN

            !-------------------------------------------------------------------------------------------
            ! #2: Get the hub height, using the strings "HUB HEIGHT" or "ZHUB"
            !-------------------------------------------------------------------------------------------

            IF ( INDEX( LINE, 'HUB HEIGHT' ) > 0 .OR. INDEX( LINE, 'ZHUB' ) > 0 ) THEN

!FIXME: convert READ to the library ones
               READ (LINE, *, IOSTAT = TmpErrStat) RefHt

               IF ( TmpErrStat /= 0 ) THEN
                  ErrMsg   = TRIM(ErrMsg)//NewLine//' Error reading hub height from FF summary file.'
                  ErrStat  = ErrID_Fatal     ! This is necessary information. Fatal if not found.
                  RETURN
               ENDIF
               StrNeeded(2) = .FALSE.

            ENDIF !INDEX for "HUB HEIGHT" or "ZHUB"

!FIXME: check that the next 6 lines were commented out originally.
   !      ELSEIF ( StrNeeded(3) ) THEN
   !
   !         !-------------------------------------------------------------------------------------------
   !         ! #3: Get the grid width (& height, if available), using the strings "GRID WIDTH" or "RDIAM"
   !         !    If GRID HEIGHT is specified, use it, too. -- THIS IS UNNECESSARY AS IT'S STORED IN THE BINARY FILE
   !         !-------------------------------------------------------------------------------------------

         ELSEIF ( StrNeeded(4) ) THEN

            !-------------------------------------------------------------------------------------------
            ! #4: Get the mean wind speed "UBAR" and turbulence intensities from following lines for
            !     scaling Bladed-style FF binary files
            !-------------------------------------------------------------------------------------------

            IF ( INDEX( LINE, 'UBAR') > 0 ) THEN

               FirstIndx = INDEX( LINE, '=' ) + 1        ! Look for the equal siqn to find the number we're looking for

!FIXME: convert READ to the library ones
               READ ( LINE( FirstIndx:LEN(LINE) ), *, IOSTAT=TmpErrStat ) MeanFFWS

               IF ( TmpErrStat /= 0 ) THEN
                  ErrMsg   =  TRIM(ErrMsg)//NewLine// &
                              ' Error reading UBar binary data normalizing parameter from FF summary file.'
                  ErrStat  =  ErrID_Fatal    ! This is necessary information. Fatal if not found.
                  RETURN
               ENDIF

               DO I = 1,3

                  LineCount = LineCount + 1

!FIXME: convert READ to the library ones
                  READ ( OtherStates%UnitWind, '(A)', IOSTAT=TmpErrStat ) LINE
                  IF ( TmpErrStat /= 0 ) THEN
                     ErrMsg   = ' Error reading line #'//TRIM(Num2LStr(LineCount))//' of the summary file, "'//TRIM(FileName)//'".'
                     ErrMsg   = TRIM(ErrMsg)//' Could not find all of the required parameters.'
                     ErrStat = ErrID_Fatal
                     RETURN
                  ENDIF

                  FirstIndx = INDEX( LINE, '=' ) + 1     ! Read the number between the = and % signs
                  LastIndx  = INDEX( LINE, '%' ) - 1

                  IF ( LastIndx <= FirstIndx ) LastIndx = LEN( LINE )   ! If there's no % sign, read to the end of the line

!FIXME: convert READ to the library ones
                  READ ( LINE( FirstIndx:LastIndx ), *, IOSTAT=TmpErrStat ) TI(I)
                  IF ( TmpErrStat /= 0 ) THEN
                     ErrMsg   =  TRIM(ErrMsg)//NewLine// &
                                 ' Error reading TI('//TRIM(Num2LStr(I))// &
                                 ') binary data normalizing parameter from FF summary file.'
                     ErrStat  = ErrID_Fatal     ! This information must be present.
                     RETURN
                  ENDIF

               ENDDO !I

               StrNeeded(4) = .FALSE.

             ENDIF

         ELSEIF ( StrNeeded(5) ) THEN

            !-------------------------------------------------------------------------------------------
            ! #5: Get the grid "HEIGHT OFFSET", if it exists (in TurbSim). Otherwise, assume it's zero
            !           ZGOffset = HH - GridBase - FFZHWid
            !-------------------------------------------------------------------------------------------
            IF ( INDEX( LINE, 'HEIGHT OFFSET' ) > 0  ) THEN

               FirstIndx = INDEX ( LINE, '=' ) + 1

!FIXME: convert READ to the library ones
               READ ( LINE( FirstIndx:LEN(LINE) ), *, IOSTAT=TmpErrStat ) ZGOffset

               IF ( TmpErrStat /= 0 ) THEN
                  ErrMsg   = TRIM(ErrMsg)//NewLine//' Error reading height offset from FF summary file.'
                  ErrStat  = ErrID_Fatal     ! Critical information
                  RETURN
               ENDIF

               StrNeeded(5) = .FALSE.

            ENDIF !INDEX for "HEIGHT OFFSET"

         ELSEIF ( StrNeeded(6) ) THEN

            !-------------------------------------------------------------------------------------------
            ! #5: Get the grid "PERIODIC", if it exists (in TurbSim). Otherwise, assume it's
            !        not a periodic file
            !-------------------------------------------------------------------------------------------
            IF ( INDEX( LINE, 'PERIODIC' ) > 0  ) THEN

               Periodic     = .TRUE.
               StrNeeded(6) = .FALSE.

            ENDIF !INDEX for "PERIODIC"

         ENDIF ! StrNeeded


      ENDDO !WHILE

      !FIXME: should this be here?
      ErrStat = 0    ! We made it to the end of the file

      !-------------------------------------------------------------------------------------------------
      ! Close the summary file
      !-------------------------------------------------------------------------------------------------

      CLOSE ( OtherStates%UnitWind )


      !-------------------------------------------------------------------------------------------------
      ! Calculate the height of the grid center
      !-------------------------------------------------------------------------------------------------

       ZCenter  = RefHt - ZGOffset


   END SUBROUTINE Read_Summary_FF
   !====================================================================================================
   SUBROUTINE Read_TurbSim_FF(UnitWind, WindFile, ErrStat, ErrMsg)
   ! This subroutine reads the binary TurbSim-format FF file (.bts).  It fills the FFData array with
   ! velocity data for the grids and fills the FFtower array with velocities at points on the tower
   ! (if data exists).
   !----------------------------------------------------------------------------------------------------

         ! Passed Variables:

      INTEGER(IntKi),                     INTENT(IN   )  :: UnitWind       ! unit number for the wind file
      CHARACTER(*),                       INTENT(IN   )  :: WindFile       ! name of the binary TurbSim file
      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat        ! error status return value (0=no error; non-zero is error)
      CHARACTER(*),                       INTENT(  OUT)  :: ErrMsg         ! message about the error encountered

         ! Local Variables:

      REAL(SiKi)                                         :: Dum_Real4      ! dummy 4-byte real number
      INTEGER(B1Ki)                                      :: Dum_Int1       ! dummy 1-byte integer
      INTEGER(B2Ki)                                      :: Dum_Int2       ! dummy 2-byte integer
      INTEGER(B4Ki)                                      :: Dum_Int4       ! dummy 4-byte integer

      INTEGER(IntKi)                                     :: IC             ! loop counter for wind components
      INTEGER(IntKi)                                     :: IT             ! loop counter for time
      INTEGER(IntKi)                                     :: IY             ! loop counter for y
      INTEGER(IntKi)                                     :: IZ             ! loop counter for z
      INTEGER(IntKi)                                     :: NChar          ! number of characters in the description string

      REAL(SiKi)                                         :: Vslope(3)      ! slope  for "un-normalizing" data
      REAL(SiKi)                                         :: Voffset(3)     ! offset for "un-normalizing" data

      CHARACTER(1024)                                    :: DescStr        ! description string contained in the file

         ! Temporary variables for error handling
      INTEGER(IntKi)                                     :: TmpErrStat     ! temporary error status
      CHARACTER(1024)                                    :: TmpErrMsg      ! temporary error message


      NFFComp = 3                                                          ! this file contains 3 wind components

   !-------------------------------------------------------------------------------------------------
   ! Open the file
   !-------------------------------------------------------------------------------------------------

      CALL OpenBInpFile (OtherStates%UnitWind, TRIM(WindFile), ErrStat, ErrMsg)
            IF ( TmpErrStat >= AbortErrLev ) THEN
               ErrStat  = MAX(TmpErrStat, ErrStat)
               ErrMsg   = TRIM(ErrMsg)//NewLine//TRIM(TmpErrMsg)
               RETURN
            ENDIF
   !!!!FIXME: Add in ErrMsg when it is available.
   !!!   IF (ErrStat /= 0) RETURN
   !!!
   !!!   !-------------------------------------------------------------------------------------------------
   !!!   ! Read the header information
   !!!   !-------------------------------------------------------------------------------------------------
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2             ! the file identifier, INT(2)
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading the file identifier in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         Periodic = Dum_Int2 == INT( 8, B2Ki) ! the number 7 is used for non-periodic wind files; 8 is periodic wind
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4             ! the number of grid points vertically, INT(4)
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading the number of z grid points in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         NZgrids = Dum_Int4
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4             ! the number of grid points laterally, INT(4)
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading the number of y grid points in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         NYgrids = Dum_Int4
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4             ! the number of tower points, INT(4)
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading the number of tower points in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         NTgrids = Dum_Int4
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4             ! the number of time steps, INT(4)
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading the number of time steps in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         NFFSteps = Dum_Int4
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! grid spacing in vertical direction (dz), REAL(4), in m
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading dz in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         InvFFZD = 1.0/Dum_Real4                            ! 1/dz
   !!!         FFZHWid = 0.5*(NZgrids-1)*Dum_Real4                ! half the grid height
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! grid spacing in lateral direction (dy), REAL(4), in m
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading dy in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         InvFFYD = 1.0 / Dum_Real4                          ! 1/dy
   !!!         FFYHWid = 0.5*(NYgrids-1)*Dum_Real4                ! half grid grid width
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! grid spacing in time (dt), REAL(4), in m/s
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading dt in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         FFDTime = Dum_Real4
   !!!         FFRate  = 1.0/FFDTime
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! the mean wind speed at hub height, REAL(4), in m/s
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading mean wind speed in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         MeanFFWS = Dum_Real4
   !!!         InvMFFWS = 1.0 / MeanFFWS
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! height of the hub, REAL(4), in m
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading zHub in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         RefHt = Dum_Real4
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4            ! height of the bottom of the grid, REAL(4), in m
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Error reading GridBase in the FF binary file "'//TRIM( WindFile )//'."'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!         GridBase = Dum_Real4
   !!!
   !!! !        ZGOffset = RefHt - GridBase  - FFZHWid
   !!!
   !!!
   !!!      !----------------------------------------------------------------------------------------------
   !!!      ! Read the binary scaling factors
   !!!      !----------------------------------------------------------------------------------------------
   !!!
   !!!         DO IC = 1,NFFComp
!FIXME: convert READ to the library ones
   !!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Vslope(IC)     ! the IC-component slope for scaling, REAL(4)
   !!!               IF ( TmpErrStat /= 0 )  THEN
   !!!                  ErrMsg   = ' Error reading Vslope('//Num2LStr(IC)//') in the FF binary file "'//TRIM( WindFile )//'."'
   !!!                  ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!                  RETURN
   !!!               ENDIF
   !!!
   !!!
!FIXME: convert READ to the library ones
   !!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Voffset(IC)    ! the IC-component offset for scaling, REAL(4)
   !!!               IF ( TmpErrStat /= 0 )  THEN
   !!!                  ErrMsg   = ' Error reading Voffset('//Num2LStr(IC)//') in the FF binary file "'//TRIM( WindFile )//'."'
   !!!                  ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!                  RETURN
   !!!               ENDIF
   !!!
   !!!         ENDDO !IC
   !!!
   !!!
   !!!      !----------------------------------------------------------------------------------------------
   !!!      ! Read the description string: "Generated by TurbSim (vx.xx, dd-mmm-yyyy) on dd-mmm-yyyy at hh:mm:ss."
   !!!      !----------------------------------------------------------------------------------------------
   !!!
!FIXME: convert READ to the library ones
   !!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4          ! the number of characters in the description string, max 200, INT(4)
   !!!            IF ( TmpErrStat /= 0 )  THEN
   !!!               ErrMsg   = ' Error reading NCHAR in the FF binary file "'//TRIM( WindFile )//'."'
   !!!               ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!               RETURN
   !!!            ENDIF
   !!!            nchar = Dum_Int4
   !!!
   !!!         DescStr = ''                                       ! Initialize the description string
   !!!
   !!!         DO IC=1,nchar
   !!!
!FIXME: convert READ to the library ones
   !!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int1       ! the ASCII integer representation of the character, INT(1)
   !!!            IF ( TmpErrStat /= 0 )  THEN
   !!!               ErrMsg   = ' Error reading description line in the FF binary file "'//TRIM( WindFile )//'."'
   !!!               ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!               RETURN
   !!!            ENDIF
   !!!
   !!!            IF ( LEN(DescStr) >= IC ) THEN
   !!!               DescStr(IC:IC) = ACHAR( Dum_Int1 )              ! converted ASCII characters
   !!!            ELSE
   !!!               ErrMsg   =  ' Description string too long.'   ! should this be handled by an ErrMsg?
   !!!               ErrStat  = ErrID_Warn
   !!!               EXIT
   !!!            ENDIF
   !!!
   !!!         ENDDO !IC
   !!!
   !!!
   !!!   !-------------------------------------------------------------------------------------------------
   !!!   ! Get the grid and tower velocities
   !!!   !-------------------------------------------------------------------------------------------------
   !!!
   !!!      CALL WrScr( NewLine//' Reading a '//TRIM( Num2LStr(NYGrids) )//'x'//TRIM( Num2LStr(NZGrids) )//  &
   !!!               ' grid ('//TRIM( Num2LStr(FFYHWid*2) )//' m wide, '// &
   !!!               TRIM( Num2LStr(GridBase) )//' m to '//TRIM( Num2LStr(GridBase+FFZHWid*2) )//&
   !!!               ' m above ground) with a characterstic wind speed of '//TRIM( Num2LStr(MeanFFWS) )//' m/s. '//TRIM(DescStr) )
   !!!
   !!!
   !!!   !----------------------------------------------------------------------------------------------
   !!!   ! Allocate arrays for the FF grid as well as the tower points, if they exist
   !!!   !----------------------------------------------------------------------------------------------
   !!!
   !!!      IF ( .NOT. ALLOCATED( FFData ) ) THEN
   !!!         ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,NFFSteps), STAT=TmpErrStat )
   !!!
   !!!         IF ( TmpErrStat /= 0 )  THEN
   !!!            ErrMsg   = ' Cannot allocate the full-field wind data array.'
   !!!            ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!            RETURN
   !!!         ENDIF
   !!!      ENDIF
   !!!
   !!!
   !!!      IF ( NTgrids > 0 ) THEN
   !!!
   !!!         IF ( .NOT. ALLOCATED( FFtower ) ) THEN
   !!!            ALLOCATE( FFtower( NFFComp, NTgrids, NFFSteps ), STAT=TmpErrStat )
   !!!
   !!!            IF ( TmpErrStat /= 0 )  THEN
   !!!               ErrMsg   = ' Cannot allocate the tower wind data array.'
   !!!               ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!               RETURN
   !!!            ENDIF
   !!!         ENDIF
   !!!
   !!!      ENDIF
   !!!
   !!!   !-------------------------------------------------------------------------------------------------
   !!!   ! Read the 16-bit data and scale it to 32-bit reals
   !!!   !-------------------------------------------------------------------------------------------------
   !!!
   !!!      ! Loop through time.
   !!!
   !!!      DO IT=1,NFFSteps
   !!!
   !!!         !...........................................................................................
   !!!         ! Read grid data at this time step.
   !!!         !...........................................................................................
   !!!
   !!!         DO IZ=1,NZgrids
   !!!            ! Zgrid(IZ) = Z1 + (IZ-1)*dz                 ! Vertical location of grid data point, in m relative to ground
   !!!
   !!!            DO IY=1,NYgrids
   !!!               ! Ygrid(IY) = -0.5*(ny-1)*dy + (IY-1)*dy  ! Horizontal location of grid data point, in m relative to tower centerline
   !!!
   !!!               DO IC=1,NFFComp                           ! number of wind components (U, V, W)
   !!!
!FIXME: convert READ to the library ones
   !!!                  READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2       ! normalized wind-component, INT(2)
   !!!                  IF ( TmpErrStat /= 0 )  THEN
   !!!                     ErrMsg   = ' Error reading grid wind components in the FF binary file "'//TRIM( WindFile )//'."'
   !!!                     ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!                     RETURN
   !!!                  ENDIF
   !!!
   !!!                  FFData(IZ,IY,IC,IT) = ( Dum_Int2 - Voffset(IC) ) / VSlope(IC)
   !!!
   !!!               ENDDO !IC
   !!!
   !!!            ENDDO !IY
   !!!
   !!!         ENDDO ! IZ
   !!!
   !!!
   !!!         !...........................................................................................
   !!!         ! Read the tower data at this time step.
   !!!         !...........................................................................................
   !!!
   !!!         DO IZ=1,NTgrids         ! If NTgrids<1, there are no tower points & FFtower is not allocated
   !!!
   !!!            ! Ytower     = 0               ! Lateral location of the tower data point, in m relative to tower centerline
   !!!            ! Ztower(IZ) = Z1 - (IZ-1)*dz  ! Vertical location of tower data point, in m relative to ground
   !!!
   !!!            DO IC=1,NFFComp   ! number of wind components
   !!!
!FIXME: convert READ to the library ones
   !!!               READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2       ! normalized wind-component, INT(2)
   !!!               IF ( TmpErrStat /= 0 )  THEN
   !!!                  ErrMsg   = ' Error reading tower wind components in the FF binary file "'//TRIM( WindFile )//'."'
   !!!                  ErrStat  = ErrID_Fatal        ! fatal since not returning data
   !!!                  RETURN
   !!!               ENDIF
   !!!
   !!!               FFtower(IC,IZ,IT) = ( Dum_Int2 - Voffset(IC) ) / VSlope(IC)  ! wind-component scaled to m/s
   !!!
   !!!            ENDDO !IC
   !!!
   !!!         ENDDO ! IZ
   !!!
   !!!
   !!!      ENDDO ! IT
   !!!
   !!!   !-------------------------------------------------------------------------------------------------
   !!!   ! close the file and return
   !!!   !-------------------------------------------------------------------------------------------------
   !!!
   !!!   CLOSE ( OtherStates%UnitWind )
   !!!
   !!!
   !!!   IF ( Periodic ) THEN
   !!!      CALL WrScr ( ' Processed '//TRIM( Num2LStr( NFFSteps ) )//' time steps of '//TRIM( Num2LStr ( FFRate ) )// &
   !!!                     '-Hz full-field data (period of '//TRIM( Num2LStr( FFDTime*( NFFSteps ) ) )//' seconds).' )
   !!!   ELSE
   !!!      CALL WrScr ( ' Processed '//TRIM( Num2LStr( NFFSteps ) )//' time steps of '//TRIM( Num2LStr ( FFRate ) )// &
   !!!                     '-Hz full-field data ('//TRIM( Num2LStr( FFDTime*( NFFSteps - 1 ) ) )//' seconds).' )
   !!!   ENDIF
   !!!
      RETURN

   END SUBROUTINE READ_TurbSim_FF

END SUBROUTINE IfW_FFWind_Init
!!!!====================================================================================================
!!!SUBROUTINE Read_Bladed_FF_Header0 (OtherStates%UnitWind, ErrStat, ErrMsg)
!!!!   Reads the binary headers from the turbulence files of the old Bladed variety.  Note that
!!!!   because of the normalization, neither NZGrids or NYGrids are larger than 32 points.
!!!!   21-Sep-2009 - B. Jonkman, NREL/NWTC.
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!
!!!   IMPLICIT                      NONE
!!!
!!!
!!!      ! Passed Variables:
!!!
!!!   INTEGER(IntKi),       INTENT(IN)  :: OtherStates%UnitWind
!!!   INTEGER(IntKi),       INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),  INTENT(OUT) :: ErrMsg
!!!
!!!      ! Local Variables:
!!!
!!!   REAL(ReKi)                 :: FFXDelt
!!!   REAL(ReKi)                 :: FFYDelt
!!!   REAL(ReKi)                 :: FFZDelt
!!!
!!!   INTEGER(B2Ki)              :: Dum_Int2
!!!
!!!   INTEGER(IntKi)                    :: I
!!!   INTEGER(IntKi)                    :: TmpErrStat                ! for checking the IOSTAT from a READ or Open statement
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Read the header (file has just been opened)
!!!   !-------------------------------------------------------------------------------------------------
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! -NFFC (file ID)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading number of wind components from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NFFComp = -1*Dum_Int2
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! delta z (mm)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dz from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFZDelt = 0.001*Dum_Int2
!!!      InvFFZD = 1.0/FFZDelt
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! delta y (mm)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dy from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFYDelt = 0.001*Dum_Int2
!!!      InvFFYD = 1.0/FFYDelt
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! delta x (mm)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dx from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFXDelt = 0.001*Dum_Int2
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! half the number of time steps
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading number of time steps from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NFFSteps = 2*Dum_Int2
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! 10 times the mean full-field wind speed
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading mean full-field wind speed from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      MeanFFWS = 0.1*Dum_Int2
!!!      InvMFFWS = 1.0/MeanFFWS
!!!      FFDTime  = FFXDelt/MeanFFWS
!!!      FFRate   = 1.0/FFDTime
!!!
!!!
!!!   DO I = 1,5
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                              ! unused variables: zLu, yLu, xLu, dummy, random seed
!!!
!!!         IF (TmpErrStat /= 0) THEN
!!!            ErrMsg   = ' Error reading 2-byte integers from binary FF file.'
!!!            ErrStat  = ErrID_Fatal     ! no data returned
!!!            RETURN
!!!         ENDIF
!!!
!!!   END DO
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! 1000*nz
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading nz from binary FF file.'
!!!         ErrStat  = ErrID_Fatal        ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NZGrids  = Dum_Int2/1000
!!!      FFZHWid  = 0.5*FFZDelt*( NZGrids - 1 )    ! half the vertical size of the grid
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! 1000*ny
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   =  ' Error reading ny from binary FF file.'
!!!         ErrStat  =  ErrID_Fatal       ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NYGrids  = Dum_Int2/1000
!!!      FFYHWid  = 0.5*FFYDelt*( NYGrids - 1 )
!!!
!!!
!!!   IF (NFFComp == 3) THEN
!!!
!!!      DO I=1,6
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                           ! unused variables: zLv, yLv, xLv, zLw, yLw, xLw
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 2-byte length scales from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!   ENDIF !NFFComp
!!!
!!!
!!!   RETURN
!!!
!!!END SUBROUTINE Read_Bladed_FF_Header0
!!!!====================================================================================================
!!!SUBROUTINE Read_Bladed_FF_Header1 (OtherStates%UnitWind, TI, ErrStat, ErrMsg)
!!!!   Reads the binary headers from the turbulence files of the new Bladed variety.
!!!!   16-May-2002 - Windward Engineering.
!!!!   21-Sep-2009 - B. Jonkman, NREL.  updated to trap errors and add extra parameters for MANN model
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!
!!!   IMPLICIT                      NONE
!!!
!!!
!!!      ! Passed Variables:
!!!
!!!   INTEGER(IntKi),       INTENT(IN)  :: OtherStates%UnitWind
!!!   REAL(ReKi),    INTENT(OUT) :: TI(3)
!!!   INTEGER(IntKi),       INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),  INTENT(OUT) :: ErrMsg
!!!
!!!      ! Local Variables:
!!!
!!!   REAL(ReKi)                 :: FFXDelt
!!!   REAL(ReKi)                 :: FFYDelt
!!!   REAL(ReKi)                 :: FFZDelt
!!!
!!!   REAL(SiKi)                 :: Dum_Real4
!!!   INTEGER(B2Ki)              :: Dum_Int2
!!!   INTEGER(B4Ki)              :: Dum_Int4
!!!
!!!   INTEGER(IntKi)                    :: I
!!!   INTEGER(IntKi)                    :: TurbType
!!!   INTEGER(IntKi)                    :: TmpErrStat
!!!
!!!
!!!   TI(:) = -1                                                                                !Initialize to -1 (not all models contain TI)
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! -99 (file ID)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading integer from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2                                                 ! turbulence type
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading turbulence type from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      TurbType = Dum_Int2
!!!
!!!
!!!   SELECT CASE (TurbType)
!!!      CASE(1, 2)
!!!         !----------------------------------------
!!!         !1-component Von Karman (1) or Kaimal (2)
!!!         !----------------------------------------
!!!            NFFComp = 1
!!!
!!!      CASE(3, 5)
!!!         !----------------------------------------
!!!         !3-component Von Karman (3) or IEC-2
!!!         ! Kaimal (5)
!!!         !----------------------------------------
!!!            NFFComp = 3
!!!
!!!      CASE(4)
!!!         !----------------------------------------
!!!         !improved Von Karman
!!!         !----------------------------------------
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                        ! number of components (should be 3)
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading number of components from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!               NFFComp = Dum_Int4
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                       ! Latitude (deg)
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading latitude from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                       ! Roughness length (m)
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading roughness length from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                       ! Reference height (m) = Z(1) + GridHeight / 2.0
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading reference height from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!
!!!
!!!            DO I = 1,3
!FIXME: convert READ to the library ones
!!!               READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                    ! TI(u, v, w) (%)
!!!
!!!                  IF (TmpErrStat /= 0) THEN
!!!                     ErrMsg   = ' Error reading TI('//'TRIM(Num2LStr(I))'//') from binary FF file.'
!!!                     ErrStat  = ErrID_Fatal     ! no data returned
!!!                     RETURN
!!!                  ENDIF
!!!                  TI(I) = Dum_Real4                                                          ! This overwrites the TI read in the summary file
!!!
!!!            END DO !I
!!!
!!!
!!!      CASE (7, 8)
!!!         !----------------------------------------
!!!         ! General Kaimal (7) or  Mann model (8)
!!!         !----------------------------------------
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                        ! number of bytes in header
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading number of header records from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!
!FIXME: convert READ to the library ones
!!!            READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                        ! number of components
!!!
!!!               IF (TmpErrStat /= 0) THEN
!!!                  ErrMsg   = ' Error reading number of data from binary FF file.'
!!!                  ErrStat  = ErrID_Fatal     ! no data returned
!!!                  RETURN
!!!               ENDIF
!!!               NFFComp = Dum_Int4
!!!
!!!
!!!      CASE DEFAULT
!!!
!!!         CALL ProgWarn( ' AeroDyn does not recognize the full-field turbulence file type ='//TRIM(Num2LStr(TurbType))//'.' )
!!!
!!!   END SELECT !TurbType
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                                ! delta z (m)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dz from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFZDelt = Dum_Real4
!!!      InvFFZD = 1.0/FFZDelt
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                               ! delta y (m)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dy from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFYDelt = Dum_Real4
!!!      InvFFYD = 1.0/FFYDelt
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                               ! delta x (m)
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading dx from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      FFXDelt = Dum_Real4
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                                ! half the number of time steps
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading number of time steps from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NFFSteps = 2*Dum_Int4
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                               ! mean full-field wind speed
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading mean full-field wind speed from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      MeanFFWS = Dum_Real4
!!!      InvMFFWS = 1.0/MeanFFWS
!!!      FFDTime  = FFXDelt/MeanFFWS
!!!      FFRate   = 1.0/FFDTime
!!!
!!!
!!!   DO I = 1,3
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                            ! unused variables: zLu, yLu, xLu
!!!
!!!         IF (TmpErrStat /= 0) THEN
!!!            ErrMsg   = ' Error reading 4-byte length scales from binary FF file.'
!!!            ErrStat  = ErrID_Fatal     ! no data returned
!!!            RETURN
!!!         ENDIF
!!!
!!!   END DO
!!!
!!!
!!!   DO I = 1,2
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                             ! unused variables: dummy, random seed
!!!
!!!         IF (TmpErrStat /= 0) THEN
!!!            ErrMsg   = ' Error reading 4-byte integers from binary FF file.'
!!!            ErrStat  = ErrID_Fatal     ! no data returned
!!!            RETURN
!!!         ENDIF
!!!
!!!   END DO
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                                ! nz
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading nz from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NZGrids  = Dum_Int4
!!!      FFZHWid  = 0.5*FFZDelt*( NZGrids - 1 )    ! half the vertical size of the grid
!!!
!!!
!FIXME: convert READ to the library ones
!!!   READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                                ! ny
!!!
!!!      IF (TmpErrStat /= 0) THEN
!!!         ErrMsg   = ' Error reading ny from binary FF file.'
!!!         ErrStat  = ErrID_Fatal     ! no data returned
!!!         RETURN
!!!      ENDIF
!!!      NYGrids  = Dum_Int4
!!!      FFYHWid  = 0.5*FFYDelt*( NYGrids - 1 )
!!!
!!!
!!!   IF (NFFComp == 3) THEN
!!!
!!!      DO I=1,6
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables: zLv, yLv, xLv, zLw, yLw, xLw
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte length scales from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!   ENDIF !NFFComp
!!!
!!!
!!!
!!!   IF ( TurbType == 7 ) THEN     ! General Kaimal model
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variable: coherence decay constant
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading coherence decay constant from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables: coherence scale parameter in m
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading coherence scale parameter from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!   ELSE IF ( TurbType == 8 ) THEN     ! Mann model
!!!
!!!      DO I=1,2
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables: shear parameter (gamma), scale length
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!      DO I=1,4
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!      DO I=1,3
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                          ! unused variables
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!      DO I=1,2
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!      DO I=1,3
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                                          ! unused variables
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!      DO I=1,2
!!!
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4                                         ! unused variables
!!!
!!!            IF (TmpErrStat /= 0) THEN
!!!               ErrMsg   = ' Error reading 4-byte parameters from binary FF file.'
!!!               ErrStat  = ErrID_Fatal     ! no data returned
!!!               RETURN
!!!            ENDIF
!!!
!!!      ENDDO !I
!!!
!!!
!!!   ENDIF !TurbType
!!!
!!!
!!!   RETURN
!!!
!!!END SUBROUTINE Read_Bladed_FF_Header1
!!!!====================================================================================================
!!!SUBROUTINE Read_Bladed_Grids ( OtherStates%UnitWind, CWise, TI, ErrStat, ErrMsg )
!!!! This subroutine continues reading OtherStates%UnitWind, starting after the headers have been read.
!!!! It reads the grids and converts the data to un-normalized wind speeds in m/s.
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!   INTEGER(IntKi),       INTENT(IN)  :: OtherStates%UnitWind
!!!   LOGICAL,       INTENT(IN)  :: CWise
!!!   REAL(ReKi),    INTENT(IN)  :: TI      (3)                  ! turbulence intensities of the wind components as defined in the FF file, not necessarially the actual TI
!!!   INTEGER(IntKi),       INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),  INTENT(OUT) :: ErrMsg
!!!
!!!!FIXME: this invokes SAVE
!!!   REAL(ReKi), PARAMETER      :: FF_Offset(3) = (/ 1.0, 0.0, 0.0 /)  ! used for "un-normalizing" the data
!!!
!!!   INTEGER(IntKi)                    :: CFirst
!!!   INTEGER(IntKi)                    :: CLast
!!!   INTEGER(IntKi)                    :: CStep
!!!   INTEGER(B2Ki)              :: Dum_Int2
!!!   INTEGER(IntKi)                    :: I
!!!   INTEGER(IntKi)                    :: IC
!!!   INTEGER(IntKi)                    :: IR
!!!   INTEGER(IntKi)                    :: IT
!!!
!!!   INTEGER(IntKi)                    :: TmpNumSteps
!!!   INTEGER(IntKi)                    :: TmpErrStat                      ! for checking the result of IOSTAT on READ or Open statements
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Generate an informative message.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   CALL WrScr( NewLine//' Reading a '//TRIM( Num2LStr(NYGrids) )//'x'//TRIM( Num2LStr(NZGrids) )//  &
!!!            ' grid ('//TRIM( Num2LStr(FFYHWid*2) )//' m wide, '// &
!!!            TRIM( Num2LStr(GridBase) )//' m to '//TRIM( Num2LStr(GridBase+FFZHWid*2) )//&
!!!            ' m above ground) with a characterstic wind speed of '//TRIM( Num2LStr(MeanFFWS) )//' m/s. ' )
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Allocate space for the FF array
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   TmpNumSteps = NFFSteps + 1       ! add another step, just in case there is an odd number of steps.
!!!
!!!!bjj: should we reorganize this FFData array so we access the data faster?
!!!
!!!   IF ( .NOT. ALLOCATED( FFData ) ) THEN
!!!      ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,TmpNumSteps),STAT=TmpErrStat )
!!!
!!!      IF ( TmpErrStat /= 0 )  THEN
!!!
!!!         ErrMsg   = ' Cannot allocate the full-field wind data array.'
!!!         ErrStat  = ErrID_Fatal     ! Since won't be able to return data
!!!         RETURN
!!!
!!!      ENDIF
!!!
!!!   ELSE
!!!      IF (SIZE(FFDATA,1) /= NZGrids .OR. SIZE(FFDATA,2) /= NYGrids .OR. &
!!!          SIZE(FFDATA,3) /= NFFComp .OR. SIZE(FFDATA,3) /= TmpNumSteps ) THEN
!!!
!!!            ! Let's make the array the correct size (we should never get here, but you never know)
!!!
!!!         DEALLOCATE( FFData )
!!!
!!!         ALLOCATE ( FFData(NZGrids,NYGrids,NFFComp,TmpNumSteps),STAT=TmpErrStat )
!!!
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!
!!!            ErrMsg   = ' Cannot allocate the full-field wind data array.'
!!!            ErrStat  =  ErrID_Fatal       ! Since won't be able to return data
!!!            RETURN
!!!
!!!         ENDIF ! Error
!!!
!!!      ENDIF !Incorrect size
!!!   ENDIF ! allocated
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Initialize the data and set column indexing to account for direction of turbine rotation (CWise)
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   FFData(:,:,:,:) = 0.0                        ! we may have only one component
!!!
!!!   IF ( CWise )  THEN
!!!      CFirst    = NYGrids
!!!      CLast     = 1
!!!      CStep     = -1
!!!   ELSE
!!!      CFirst    = 1
!!!      CLast     = NYGrids
!!!      CStep     = 1
!!!   ENDIF
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Loop through all the time steps, reading the data and converting to m/s
!!!   !-------------------------------------------------------------------------------------------------
!!!!bjj: should we reorganize this FFData array so we access the data faster?
!!!
!!!   NFFSteps = TmpNumSteps
!!!
!!!TIME_LOOP:  DO IT=1,TmpNumSteps     ! time (add 1 to see if there is an odd number of grids)
!!!
!!!      DO IR=1,NZGrids               ! the rows (vertical)
!!!
!!!         DO IC=CFirst,CLast,CStep   ! the columns (lateral)
!!!
!!!            DO I=1,NFFComp          ! wind components (U, V, W)
!!!
!FIXME: convert READ to the library ones
!!!               READ (OtherStates%UnitWind,IOStat=TmpErrStat)  Dum_Int2
!!!               IF (TmpErrStat /= 0) THEN
!!!                  IF ( IT == TmpNumSteps ) THEN ! There really were an even number of steps
!!!                     NFFSteps = TmpNumSteps - 1
!!!                     ErrStat  = 0
!!!                     EXIT TIME_LOOP
!!!                  ELSE
!!!
!!!                     ErrMsg   = ' Error reading binary data file. ic = '//TRIM(Num2LStr(ic))// &
!!!                                    ', ir = '//TRIM(Num2LStr(ir))//', it = '//TRIM(Num2LStr(it))// &
!!!                                    ', nffsteps = '//TRIM(Num2LStr(nffsteps))
!!!                     ErrStat  = ErrID_Fatal        ! since can't return data
!!!                     RETURN
!!!                  ENDIF
!!!               ELSE
!!!                  FFData(IR,IC,I,IT) = MeanFFWS*(FF_Offset(I)+0.00001*TI(I)*Dum_Int2)
!!!               ENDIF
!!!
!!!            END DO !I
!!!
!!!         END DO !IC
!!!
!!!      END DO !IR
!!!
!!!   END DO TIME_LOOP !IT
!!!
!!!   IF ( Periodic ) THEN
!!!      CALL WrScr ( ' Processed '//TRIM( Num2LStr( NFFSteps ) )//' time steps of '//TRIM( Num2LStr ( FFRate ) )// &
!!!                     '-Hz full-field data (period of '//TRIM( Num2LStr( FFDTime*NFFSteps ) )//' seconds).' )
!!!   ELSE
!!!      CALL WrScr ( ' Processed '//TRIM( Num2LStr( NFFSteps ) )//' time steps of '//TRIM( Num2LStr ( FFRate ) )// &
!!!                     '-Hz full-field data ('//TRIM( Num2LStr( FFDTime*( NFFSteps - 1 ) ) )//' seconds).' )
!!!   ENDIF
!!!
!!!END SUBROUTINE Read_Bladed_Grids
!!!!====================================================================================================
!!!SUBROUTINE Read_FF_Tower( OtherStates%UnitWind, WindFile, ErrStat, ErrMsg )
!!!! This subroutine reads the binary tower file that corresponds with the Bladed-style FF binary file.
!!!! The FF grid must be read before this subroutine is called! (many checks are made to ensure the
!!!! files belong together)
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!      ! Passed Variables:
!!!
!!!   INTEGER(IntKi),       INTENT(IN)  :: OtherStates%UnitWind            ! unit number for the wind file
!!!   CHARACTER(*),  INTENT(IN)  :: WindFile          ! name of the binary TurbSim file
!!!   INTEGER(IntKi),       INTENT(OUT) :: ErrStat           ! error status return value (0=no error; non-zero is error)
!!!   CHARACTER(*),  INTENT(OUT) :: ErrMsg            ! a message for errors that occur
!!!
!!!      ! Local Variables:
!!!
!!!   REAL(SiKi)                 :: Dum_Real4         ! dummy 4-byte real number
!!!   INTEGER(B2Ki)              :: Dum_Int2          ! dummy 2-byte integer
!!!   INTEGER(B4Ki)              :: Dum_Int4          ! dummy 4-byte integer
!!!
!!!   INTEGER(IntKi)                    :: IC                ! loop counter for wind components
!!!   INTEGER(IntKi)                    :: IT                ! loop counter for time
!!!   INTEGER(IntKi)                    :: IZ                ! loop counter for z
!!!   INTEGER(IntKi)                    :: TmpErrStat          ! IOSTAT value.
!!!
!!!!FIXME: this invokes SAVE
!!!   REAL(ReKi), PARAMETER      :: TOL = 1E-4        ! tolerence for wind file comparisons
!!!
!!!!FIXME: this invokes SAVE
!!!   REAL(ReKi), PARAMETER      :: FF_Offset(3) = (/ 1.0, 0.0, 0.0 /)  ! used for "un-normalizing" the data
!!!   REAL(SiKi)                 :: TI       (3)      ! scaling values for "un-normalizing the data" [approx. turbulence intensities of the wind components]
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   !
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   NTgrids = 0
!!!
!!!   IF ( NFFComp /= 3 ) THEN
!!!      ErrMsg   = ' Error: Tower binary files require 3 wind components.'
!!!      ErrStat  = ErrID_Fatal     ! was '= 1'
!!!      RETURN
!!!   ENDIF
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Open the file
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   CALL OpenBInpFile (OtherStates%UnitWind, TRIM(WindFile), TmpErrStat, ErrMsg)
!!!!FIXME: Add ErrMsg when it is available.
!!!   IF (TmpErrStat /= 0) THEN
!!!      ErrMsg   = 'Error opening file '//TRIM(WindFile)
!!!      TmpErrStat = ErrID_Fatal        ! was '= -1'
!!!      RETURN
!!!   ENDIF
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Read the header information and check that it's compatible with the FF Bladed-style binary
!!!   ! parameters already read.
!!!   !-------------------------------------------------------------------------------------------------
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4               ! dz, in meters [4-byte REAL]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading dz in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat  = ErrID_Fatal     ! wasn't a value before
!!!            RETURN
!!!         ENDIF
!!!
!!!         IF ( ABS(Dum_Real4*InvFFZD-1) > TOL ) THEN
!!!            ErrMsg   = ' Resolution in the FF binary file does not match the tower file.'
!!!            ErrStat  = ErrID_Fatal     ! was '= 1' -- should this be fatal, or just a warning?
!!!            RETURN
!!!         ENDIF
!!!
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4               ! dx, in meters [4-byte REAL]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading dx in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat  = ErrID_Fatal     ! fatal since returning without data
!!!            RETURN
!!!         ENDIF
!!!
!!!         IF ( ABS(Dum_Real4*InvMFFWS/FFDTime-1) > TOL ) THEN
!!!            ErrMsg   = ' Time resolution in the FF binary file does not match the tower file.'
!!!            ErrStat  = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4               ! Zmax, in meters [4-byte REAL]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading GridBase in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat  = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!         IF ( ABS(Dum_Real4/GridBase-1) > TOL ) THEN
!!!            ErrMsg   = ' Height in the FF binary file does not match the tower file.'
!!!            ErrStat = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                ! NumOutSteps [4-byte INTEGER]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading NumOutSteps in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!         IF ( Dum_Int4 /= NFFSteps ) THEN
!!!            ErrMsg   = ' Number of time steps in the FF binary file does not match the tower file.'
!!!            ErrStat = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int4                ! NumZ      [4-byte INTEGER]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading NumZ in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!         NTgrids = Dum_Int4
!!!
!!!
!FIXME: convert READ to the library ones
!!!      READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Real4               ! UHub      [4-byte REAL]
!!!         IF ( TmpErrStat /= 0 )  THEN
!!!            ErrMsg   = ' Error reading UHub in the binary tower file "'//TRIM( WindFile )//'."'
!!!            ErrStat = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!
!!!         IF ( ABS(Dum_Real4*InvMFFWS - 1) > TOL ) THEN
!!!            ErrMsg   = ' Mean wind speed in the FF binary file does not match the tower file.'
!!!            ErrStat = ErrID_Fatal
!!!            NTgrids = 0
!!!            RETURN
!!!         ENDIF
!!!
!!!
!!!      DO IC=1,3
!FIXME: convert READ to the library ones
!!!         READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   TI(IC)               ! TI(u), TI(v), TI(w)  [4-byte REAL]
!!!            IF ( TmpErrStat /= 0 )  THEN
!!!               ErrMsg   = ' Error reading TI('//TRIM(Num2LStr(IC))//') in the binary tower file "' &
!!!                               //TRIM( WindFile )//'."'
!!!               ErrStat  = ErrID_Fatal
!!!               NTgrids  = 0
!!!               RETURN
!!!            ENDIF
!!!      END DO
!!!
!!!   !----------------------------------------------------------------------------------------------
!!!   ! Allocate arrays for the tower points
!!!   !----------------------------------------------------------------------------------------------
!!!
!!!      IF ( NTgrids > 0 ) THEN
!!!
!!!         IF ( .NOT. ALLOCATED( FFtower ) ) THEN
!!!!            CALL AllocAry( FFtower, NFFComp, NTgrids, NFFSteps, 'tower wind data', ErrStat )
!!!            ALLOCATE ( FFtower(NFFComp,NTgrids,NFFSteps), STAT=TmpErrStat )
!!!
!!!            IF ( TmpErrStat /= 0 )  THEN
!!!               ErrMsg   = ' Error allocating memory for the tower wind data array.'
!!!               ErrStat = ErrID_Fatal
!!!               NTgrids = 0
!!!               RETURN
!!!            ENDIF
!!!
!!!         ELSE
!!!            ! Check sizes here!
!!!         ENDIF
!!!
!!!      ENDIF
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Read the 16-bit time-series data and scale it to 32-bit reals
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!      ! Loop through time.
!!!
!!!      DO IT=1,NFFSteps
!!!
!!!         DO IZ=1,NTgrids         ! If NTgrids<1, there are no tower points & FFtower is not allocated
!!!
!!!            ! Ytower     = 0               ! Lateral location of the tower data point, in m relative to tower centerline
!!!            ! Ztower(IZ) = Z1 - (IZ-1)*dz  ! Vertical location of tower data point, in m relative to ground
!!!
!!!            DO IC=1,NFFComp   ! number of wind components
!!!
!FIXME: convert READ to the library ones
!!!               READ (OtherStates%UnitWind, IOSTAT=TmpErrStat)   Dum_Int2       ! normalized wind-component, INT(2)
!!!               IF ( TmpErrStat /= 0 )  THEN
!!!                  ErrMsg   = ' Error reading binary tower data file. it = '//TRIM(Num2LStr(it))// &
!!!                                 ', nffsteps = '//TRIM(Num2LStr(nffsteps))
!!!                  ErrStat  = ErrID_Fatal
!!!                  NTgrids  = 0
!!!                  RETURN
!!!               ENDIF
!!!
!!!               FFtower(IC,IZ,IT) = MeanFFWS*(FF_Offset(IC)+0.00001*TI(IC)*Dum_Int2)   ! wind-component scaled to m/s
!!!
!!!            ENDDO !IC
!!!
!!!         ENDDO ! IZ
!!!
!!!
!!!      ENDDO ! IT
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Close the file
!!!   !-------------------------------------------------------------------------------------------------
!!!   CLOSE ( OtherStates%UnitWind )
!!!
!!!
!!!   CALL WrScr ( ' Processed '//TRIM( Num2LStr(NFFSteps) )//' time steps of '//TRIM( Num2LStr(NTgrids) )//'x1 tower data grids.')
!!!
!!!
!!!   RETURN
!!!
!!!END SUBROUTINE Read_FF_Tower
!!!!====================================================================================================
!!!FUNCTION FF_GetRValue(RVarName, ErrStat, ErrMsg)
!!!!  This function returns a real scalar value whose name is listed in the RVarName input argument.
!!!!  If the name is not recognized, an error is returned in ErrStat.
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!   CHARACTER(*),     INTENT(IN)  :: RVarName
!!!   INTEGER(IntKi),          INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),     INTENT(OUT) :: ErrMsg
!!!   REAL(ReKi)                    :: FF_GetRValue
!!!
!!!
!!!   CHARACTER(20)                 :: VarNameUC
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Check that the module has been initialized.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   IF ( .NOT. Initialized ) THEN
!!!      ErrMsg   = ' Initialialize the FFWind module before calling its subroutines.'
!!!      ErrStat  = ErrID_Fatal      ! was '= 1'
!!!      RETURN
!!!   ELSE
!!!      ErrStat = 0
!!!   ENDIF
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Return the requested values.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   VarNameUC = RVarName
!!!   CALL Conv2UC( VarNameUC )
!!!
!!!   SELECT CASE ( TRIM(VarNameUC) )
!!!
!!!      CASE ('HUBHEIGHT', 'REFHEIGHT' )
!!!         FF_GetRValue = RefHt
!!!
!!!      CASE ('GRIDWIDTH', 'FFYWID' )
!!!         FF_GetRValue = FFYHWid*2
!!!
!!!      CASE ('GRIDHEIGHT', 'FFZWID' )
!!!         FF_GetRValue = FFZHWid*2
!!!
!!!      CASE ('MEANFFWS' )
!!!         FF_GetRValue = MeanFFWS
!!!
!!!      CASE DEFAULT
!!!         ErrMsg   = ' Invalid variable name in FF_GetRValue().'
!!!         ErrStat  = ErrID_Fatal     ! was '= 1'
!!!
!!!   END SELECT
!!!
!!!END FUNCTION FF_GetRValue
!!!!====================================================================================================
!!!FUNCTION FF_GetWindSpeed(Time, InputPosition, ErrStat, ErrMsg)
!!!! This function receives time and position (in InputInfo) where (undisturbed) velocities are are
!!!! requested.  It determines if the point is on the FF grid or tower points and calls the
!!!! corresponding interpolation routine, which returns the velocities at the specified time and space.
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!   REAL(DbKi),       INTENT(IN)  :: Time
!!!   REAL(ReKi),       INTENT(IN)  :: InputPosition(3)
!!!   INTEGER(IntKi),          INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),     INTENT(OUT) :: ErrMsg
!!!
!!!   REAL(ReKi)                    :: FF_GetWindSpeed(3)
!!!
!!!!FIXME: this invokes SAVE
!!!   REAL(ReKi), PARAMETER         :: TOL = 1E-3
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Check that the module has been initialized.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   IF ( .NOT. Initialized ) THEN
!!!      ErrMsg   = ' Initialialize the FFWind module before calling its subroutines.'
!!!      ErrStat  = ErrID_Fatal     ! was '= 1'
!!!      RETURN
!!!   ELSE
!!!      ErrStat = 0
!!!   ENDIF
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Find out if the location is on the grid on on tower points; interpolate and return the value.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!    FF_GetWindSpeed = FF_Interp(Time,InputPosition, ErrStat, ErrMsg)
!!!
!!!
!!!!   IF ( InputPosition(3) >= GridBase - TOL ) THEN
!!!!
!!!!         ! Get the velocities interpolated on the FF grid
!!!!
!!!!      FF_GetWindSpeed%Velocity = FF_Interp(Time,InputPosition, ErrStat)
!!!!
!!!!   ELSE
!!!!
!!!!         ! Get the velocities interpolated below the FF grid, on the tower points
!!!!
!!!!      IF ( NTgrids < 1 ) THEN
!!!!
!!!!         ErrMsg   = ' Error: FF interpolation height is below the grid and no tower points have been defined.'
!!!!         ErrStat = ErrID_Fatal        ! was '= 1'
!!!!         RETURN
!!!!
!!!!      ELSE
!!!!
!!!!         FF_GetWindSpeed%Velocity = FF_TowerInterp(Time,InputInfo%Position, ErrStat)
!!!!
!!!!      ENDIF   ! NTgrids < 1
!!!!
!!!!
!!!!   ENDIF      ! InputInfo%Position(3)>= GridBase
!!!
!!!
!!!END FUNCTION FF_GetWindSpeed
!!!!====================================================================================================
!!!FUNCTION FF_Interp(Time, Position, ErrStat, ErrMsg)
!!!!    This function is used to interpolate into the full-field wind array or tower array if it has
!!!!    been defined and is necessary for the given inputs.  It receives X, Y, Z and
!!!!    TIME from the calling routine.  It then computes a time shift due to a nonzero X based upon
!!!!    the average windspeed.  The modified time is used to decide which pair of time slices to interpolate
!!!!    within and between.  After finding the two time slices, it decides which four grid points bound the
!!!!    (Y,Z) pair.  It does a bilinear interpolation for each time slice. Linear interpolation is then used
!!!!    to interpolate between time slices.  This routine assumes that X is downwind, Y is to the left when
!!!!    looking downwind and Z is up.  It also assumes that no extrapolation will be needed.
!!!!
!!!!    If tower points are used, it assumes the velocity at the ground is 0.  It interpolates between
!!!!    heights and between time slices, but ignores the Y input.
!!!!
!!!!    11/07/94 - Created by M. Buhl from the original TURBINT.
!!!!    09/25/97 - Modified by M. Buhl to use f90 constructs and new variable names.  Renamed to FF_Interp.
!!!!    09/23/09 - Modified by B. Jonkman to use arguments instead of modules to determine time and position.
!!!!               Height is now relative to the ground
!!!!
!!!!----------------------------------------------------------------------------------------------------
!!!
!!!   IMPLICIT                      NONE
!!!
!!!   REAL(ReKi),       INTENT(IN)  :: Position(3)       ! takes the place of XGrnd, YGrnd, ZGrnd
!!!   REAL(DbKi),       INTENT(IN)  :: Time
!!!   REAL(ReKi)                    :: FF_Interp(3)      ! The U, V, W velocities
!!!
!!!   INTEGER(IntKi),          INTENT(OUT) :: ErrStat
!!!   CHARACTER(*),     INTENT(OUT) :: ErrMsg
!!!
!!!      ! Local Variables:
!!!
!!!   REAL(ReKi)                    :: TimeShifted
!!!!FIXME: this invokes SAVE
!!!   REAL(ReKi),PARAMETER          :: Tol = 1.0E-3      ! a tolerance for determining if two reals are the same (for extrapolation)
!!!   REAL(ReKi)                    :: W_YH_Z
!!!   REAL(ReKi)                    :: W_YH_ZH
!!!   REAL(ReKi)                    :: W_YH_ZL
!!!   REAL(ReKi)                    :: W_YL_Z
!!!   REAL(ReKi)                    :: W_YL_ZH
!!!   REAL(ReKi)                    :: W_YL_ZL
!!!   REAL(ReKi)                    :: Wnd(2)
!!!   REAL(ReKi)                    :: T
!!!   REAL(ReKi)                    :: TGRID
!!!   REAL(ReKi)                    :: Y
!!!   REAL(ReKi)                    :: YGRID
!!!   REAL(ReKi)                    :: Z
!!!   REAL(ReKi)                    :: ZGRID
!!!
!!!   INTEGER(IntKi)                       :: IDIM
!!!   INTEGER(IntKi)                       :: IG
!!!   INTEGER(IntKi)                       :: IT
!!!   INTEGER(IntKi)                       :: ITHI
!!!   INTEGER(IntKi)                       :: ITLO
!!!   INTEGER(IntKi)                       :: IYHI
!!!   INTEGER(IntKi)                       :: IYLO
!!!   INTEGER(IntKi)                       :: IZHI
!!!   INTEGER(IntKi)                       :: IZLO
!!!
!!!   LOGICAL                       :: OnGrid
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Initialize variables
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   FF_Interp(:)          = 0.0                         ! the output velocities (in case NFFComp /= 3)
!!!   Wnd(:)                = 0.0                         ! just in case we're on an end point
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Find the bounding time slices.
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   ! Perform the time shift.  At time=0, a point half the grid width downstream (FFYHWid) will index into the zero time slice.
!!!   ! If we did not do this, any point downstream of the tower at the beginning of the run would index outside of the array.
!!!   ! This all assumes the grid width is at least as large as the rotor.  If it isn't, then the interpolation will not work.
!!!
!!!
!!!   TimeShifted = TIME + ( InitXPosition - Position(1) )*InvMFFWS    ! in distance, X: InputInfo%Position(1) - InitXPosition - TIME*MeanFFWS
!!!
!!!
!!!   IF ( Periodic ) THEN ! translate TimeShifted to ( 0 <= TimeShifted < TotalTime )
!!!
!!!      TimeShifted = MODULO( TimeShifted, TotalTime )
!!!
!!!      TGRID = TimeShifted*FFRate
!!!      ITLO  = INT( TGRID )             ! convert REAL to INTEGER (add 1 later because our grids start at 1, not 0)
!!!      T     = TGRID - ITLO             ! a value between 0 and 1 that indicates a relative location between ITLO and ITHI
!!!
!!!      ITLO = ITLO + 1
!!!      IF ( ITLO == NFFSteps ) THEN
!!!         ITHI = 1
!!!      ELSE
!!!         ITHI = ITLO + 1
!!!      ENDIF
!!!
!!!
!!!   ELSE
!!!
!!!      TGRID = TimeShifted*FFRate
!!!      ITLO  = INT( TGRID )             ! convert REAL to INTEGER (add 1 later because our grids start at 1, not 0)
!!!      T     = TGRID - ITLO             ! a value between 0 and 1 that indicates a relative location between ITLO and ITHI
!!!
!!!      ITLO = ITLO + 1                  ! add one since our grids start at 1, not 0
!!!      ITHI = ITLO + 1
!!!
!!!      IF ( ITLO >= NFFSteps .OR. ITLO < 1 ) THEN
!!!         IF ( ITLO == NFFSteps  ) THEN
!!!            ITHI = ITLO
!!!            IF ( T <= TOL ) THEN ! we're on the last point
!!!               T = 0.0
!!!            ELSE  ! We'll extrapolate one dt past the last value in the file
!!!               ITLO = ITHI - 1
!!!            ENDIF
!!!         ELSE
!!!            ErrMsg   = ' Error: FF wind array was exhausted at '//TRIM( Num2LStr( REAL( TIME,   ReKi ) ) )// &
!!!                       ' seconds (trying to access data at '//TRIM( Num2LStr( REAL( TimeShifted, ReKi ) ) )//' seconds).'
!!!            ErrStat  = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!      ENDIF
!!!
!!!   ENDIF
!!!
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Find the bounding rows for the Z position. [The lower-left corner is (1,1) when looking upwind.]
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!   ZGRID = ( Position(3) - GridBase )*InvFFZD
!!!
!!!   IF (ZGRID > -1*TOL) THEN
!!!      OnGrid = .TRUE.
!!!
!!!      IZLO = INT( ZGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!!!      IZHI = IZLO + 1
!!!
!!!      Z = ZGRID - ( IZLO - 1 )            ! a value between 0 and 1 that indicates a relative location between IZLO and IZHI
!!!
!!!      IF ( IZLO < 1 ) THEN
!!!         IF ( IZLO == 0 .AND. Z >= 1.0-TOL ) THEN
!!!            Z    = 0.0
!!!            IZLO = 1
!!!         ELSE
!!!            ErrMsg   = ' Error: FF wind array boundaries violated. Grid too small in Z direction (Z='//&
!!!                       TRIM(Num2LStr(Position(3)))//' m is below the grid).'
!!!            ErrStat  = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!      ELSEIF ( IZLO >= NZGrids ) THEN
!!!         IF ( IZLO == NZGrids .AND. Z <= TOL ) THEN
!!!            Z    = 0.0
!!!            IZHI = IZLO                   ! We're right on the last point, which is still okay
!!!         ELSE
!!!            ErrMsg   = ' Error: FF wind array boundaries violated. Grid too small in Z direction (Z='//&
!!!                       TRIM(Num2LStr(Position(3)))//' m is above the grid).'
!!!            ErrStat  = ErrID_Fatal
!!!            RETURN
!!!         ENDIF
!!!      ENDIF
!!!
!!!   ELSE
!!!
!!!      OnGrid = .FALSE.  ! this is on the tower
!!!
!!!      IF ( NTGrids < 1 ) THEN
!!!         ErrMsg   = ' Error: FF wind array boundaries violated. Grid too small in Z direction '// &
!!!                    '(height (Z='//TRIM(Num2LStr(Position(3)))//' m) is below the grid and no tower points are defined).'
!!!         ErrStat  = ErrID_Fatal
!!!         RETURN
!!!      ENDIF
!!!
!!!      IZLO = INT( -1.0*ZGRID ) + 1            ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!!!
!!!
!!!      IF ( IZLO >= NTGrids ) THEN  !our dz is the difference between the bottom tower point and the ground
!!!         IZLO = NTGrids
!!!
!!!         Z    = 1.0 - Position(3) / (GridBase - (IZLO-1)/InvFFZD) !check that this isn't 0
!!!      ELSE
!!!         Z    = ABS(ZGRID) - (IZLO - 1)
!!!      ENDIF
!!!      IZHI = IZLO + 1
!!!
!!!   ENDIF
!!!
!!!
!!!   IF ( OnGrid ) THEN      ! The tower points don't use this
!!!
!!!      !-------------------------------------------------------------------------------------------------
!!!      ! Find the bounding columns for the Y position. [The lower-left corner is (1,1) when looking upwind.]
!!!      !-------------------------------------------------------------------------------------------------
!!!
!!!         YGRID = ( Position(2) + FFYHWid )*InvFFYD    ! really, it's (Position(2) - -1.0*FFYHWid)
!!!
!!!         IYLO = INT( YGRID ) + 1             ! convert REAL to INTEGER, then add one since our grids start at 1, not 0
!!!         IYHI = IYLO + 1
!!!
!!!         Y    = YGRID - ( IYLO - 1 )         ! a value between 0 and 1 that indicates a relative location between IYLO and IYHI
!!!
!!!         IF ( IYLO >= NYGrids .OR. IYLO < 1 ) THEN
!!!            IF ( IYLO == 0 .AND. Y >= 1.0-TOL ) THEN
!!!               Y    = 0.0
!!!               IYLO = 1
!!!            ELSE IF ( IYLO == NYGrids .AND. Y <= TOL ) THEN
!!!               Y    = 0.0
!!!               IYHI = IYLO                   ! We're right on the last point, which is still okay
!!!            ELSE
!!!               ErrMsg   = ' Error FF wind array boundaries violated: Grid too small in Y direction. Y='// &
!!!                          TRIM(Num2LStr(Position(2)))//'; Y boundaries = ['//TRIM(Num2LStr(-1.0*FFYHWid))// &
!!!                          ', '//TRIM(Num2LStr(FFYHWid))//']'
!!!               ErrStat = 2
!!!               RETURN
!!!            ENDIF
!!!         ENDIF
!!!
!!!      !-------------------------------------------------------------------------------------------------
!!!      ! Interpolate on the grid
!!!      !-------------------------------------------------------------------------------------------------
!!!
!!!      DO IDIM=1,NFFComp       ! all the components
!!!
!!!         IT = ITLO            ! Start using the ITLO slice
!!!
!!!         DO IG=1,2            ! repeat for 2 time slices (by changing the value of IT. note that we can't loop from IXLO to IXHI because they could be NFFSteps and 1 respectively)
!!!
!!!            !-------------------------------------------------------------------------------------------
!!!            ! Get the wind velocity values for the four corners of the grid for this time.
!!!            !-------------------------------------------------------------------------------------------
!!!
!!!            W_YL_ZL = FFData( IZLO, IYLO, IDIM, IT )
!!!            W_YL_ZH = FFData( IZHI, IYLO, IDIM, IT )
!!!            W_YH_ZL = FFData( IZLO, IYHI, IDIM, IT )
!!!            W_YH_ZH = FFData( IZHI, IYHI, IDIM, IT )
!!!
!!!
!!!            !-------------------------------------------------------------------------------------------
!!!            ! Interpolate within the grid for this time.
!!!            !-------------------------------------------------------------------------------------------
!!!
!!!            W_YL_Z  = ( W_YL_ZH - W_YL_ZL )*Z + W_YL_ZL
!!!            W_YH_Z  = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL
!!!            Wnd(IG) = ( W_YH_Z  - W_YL_Z  )*Y + W_YL_Z
!!!
!!!            IT = ITHI            ! repeat for the using the ITHI slice
!!!
!!!         END DO !IG
!!!
!!!         !----------------------------------------------------------------------------------------------
!!!         ! Interpolate between the two times.
!!!         !----------------------------------------------------------------------------------------------
!!!
!!!         FF_Interp(IDIM) = ( Wnd(2) - Wnd(1) ) * T + Wnd(1)    ! interpolated velocity
!!!
!!!      END DO !IDIM
!!!
!!!   ELSE
!!!
!!!   !-------------------------------------------------------------------------------------------------
!!!   ! Interpolate on the tower array
!!!   !-------------------------------------------------------------------------------------------------
!!!
!!!      DO IDIM=1,NFFComp    ! all the components
!!!
!!!         IT = ITLO            ! Start using the ITLO slice
!!!
!!!         DO IG=1,2            ! repeat for 2 time slices (by changing the value of IT. note that we can't loop from IXLO to IXHI because they could be NFFSteps and 1 respectively)
!!!
!!!            !-------------------------------------------------------------------------------------------
!!!            ! Get the wind velocity values for the two corners of the grid for this time.
!!!            !-------------------------------------------------------------------------------------------
!!!
!!!            W_YH_ZL = FFTower( IDIM, IZLO, IT )
!!!
!!!            IF ( IZHI > NTGrids ) THEN
!!!               W_YH_ZH = 0.0
!!!            ELSE
!!!               W_YH_ZH = FFTower( IDIM, IZHI, IT )
!!!            ENDIF
!!!
!!!
!!!            !-------------------------------------------------------------------------------------------
!!!            ! Interpolate within the grid for this time.
!!!            !-------------------------------------------------------------------------------------------
!!!
!!!            Wnd(IG) = ( W_YH_ZH - W_YH_ZL )*Z + W_YH_ZL
!!!
!!!            IT = ITHI            ! repeat for the using the ITHI slice
!!!
!!!         END DO !IG
!!!
!!!         !----------------------------------------------------------------------------------------------
!!!         ! Interpolate between the two times.
!!!         !----------------------------------------------------------------------------------------------
!!!
!!!         FF_Interp(IDIM) = ( Wnd(2) - Wnd(1) ) * T + Wnd(1)    ! interpolated velocity
!!!
!!!      END DO !IDIM
!!!
!!!   ENDIF ! OnGrid
!!!
!!!   RETURN
!!!
!!!END FUNCTION FF_Interp
!====================================================================================================
SUBROUTINE IfW_FFWind_End( InData,     ParamData,                                &
                           ContStates, DiscStates, ConstrStates,  OtherStates,   &
                           OutData,                                              &
                           ErrStat,    ErrMsg)
!  This subroutine cleans up any data that is still allocated.  The (possibly) open files are
!  closed in InflowWindMod.
!----------------------------------------------------------------------------------------------------

      ! Passed Variables
   TYPE(IfW_FFWind_InputType),            INTENT(INOUT)  :: InData         ! Initialized input data variable
   TYPE(IfW_FFWind_ParameterType),        INTENT(INOUT)  :: ParamData      ! Parameters
   TYPE(IfW_FFWind_ContinuousStateType),  INTENT(INOUT)  :: ContStates     ! Continuous States  (unused)
   TYPE(IfW_FFWind_DiscreteStateType),    INTENT(INOUT)  :: DiscStates     ! Discrete States    (unused)
   TYPE(IfW_FFWind_ConstraintStateType),  INTENT(INOUT)  :: ConstrStates   ! Constraint States  (unused)
   TYPE(IfW_FFWind_OtherStateType),       INTENT(INOUT)  :: OtherStates    ! Other State data   (storage for the main data)
   TYPE(IfW_FFWind_OutputType),           INTENT(INOUT)  :: OutData        ! Initial output


      ! Error Handling
   INTEGER(IntKi),                        INTENT(OUT)    :: ErrStat        ! determines if an error has been encountered
   CHARACTER(1024),                       INTENT(OUT)    :: ErrMsg         ! Message about errors


      ! Local Variables
   INTEGER(IntKi)                                        :: TmpErrStat     ! temporary error status
   CHARACTER(1024)                                       :: TmpErrMsg      ! temporary error message


      !-=- Initialize the routine -=-

   ErrMsg   = ''
   ErrStat  = ErrID_None

   IF ( ALLOCATED( OtherStates%FFData  ) )   DEALLOCATE( OtherStates%FFData,  STAT=TmpErrStat )
   IF ( TmpErrStat /=0 ) THEN
      ErrMsg   = TRIM(ErrMsg)//NewLine//'IfW_FFWind_End: could not deallocate FFData array'
      ErrStat  = MAX(ErrStat, ErrID_Severe)
   ENDIF

   IF ( ALLOCATED( OtherStates%FFTower ) )   DEALLOCATE( OtherStates%FFTower, STAT=TmpErrStat )
   IF ( TmpErrStat /=0 ) THEN
      ErrMsg   = TRIM(ErrMsg)//NewLine//'IfW_FFWind_End: could not deallocate FFTower array'
      ErrStat  = MAX(ErrStat, ErrID_Severe)
   ENDIF

      ! flag as uninitialized
   ParamData%Initialized = .FALSE.


END SUBROUTINE IfW_FFWind_End

!====================================================================================================

!====================================================================================================


!====================================================================================================
! The following are generic routines required by the framework.
!====================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IfW_FFWind_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete states
! Constraint states are solved for input Time; Continuous and discrete states are updated for Time + Interval
!..................................................................................................................................

      REAL(DbKi),                            INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(IfW_FFWind_InputType),            INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(IfW_FFWind_ParameterType),        INTENT(IN   )  :: p           ! Parameters
      TYPE(IfW_FFWind_ContinuousStateType),  INTENT(INOUT)  :: x           ! Input: Continuous states at Time;
                                                                           ! Output: Continuous states at Time + Interval
      TYPE(IfW_FFWind_DiscreteStateType),    INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                           ! Output: Discrete states at Time  + Interval
      TYPE(IfW_FFWind_ConstraintStateType),  INTENT(INOUT)  :: z           ! Input: Initial guess of constraint states at Time;
                                                                           ! Output: Constraint states at Time
      TYPE(IfW_FFWind_OtherStateType),       INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                        INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                          INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None

         ! Local variables

      TYPE(IfW_FFWind_ContinuousStateType)                  :: dxdt        ! Continuous state derivatives at Time
      TYPE(IfW_FFWind_ConstraintStateType)                  :: z_Residual  ! Residual of the constraint state equations (Z)

      INTEGER(IntKi)                                        :: ErrStat2    ! Error status of the operation (occurs after initial error)
      CHARACTER(LEN(ErrMsg))                                :: ErrMsg2     ! Error message if ErrStat2 /= ErrID_None

         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""



         ! Solve for the constraint states (z) here:

         ! Check if the z guess is correct and update z with a new guess.
         ! Iterate until the value is within a given tolerance.

      CALL IfW_FFWind_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL IfW_FFWind_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      ENDIF

      ! DO WHILE ( z_Residual% > tolerance )
      !
      !  z =
      !
      !  CALL IfW_FFWind_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_Residual, ErrStat, ErrMsg )
      !  IF ( ErrStat >= AbortErrLev ) THEN
      !     CALL IfW_FFWind_DestroyConstrState( z_Residual, ErrStat2, ErrMsg2)
      !     ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
      !     RETURN
      !  ENDIF
      !
      ! END DO


         ! Destroy z_Residual because it is not necessary for the rest of the subroutine:

      CALL IfW_FFWind_DestroyConstrState( z_Residual, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



         ! Get first time derivatives of continuous states (dxdt):

      CALL IfW_FFWind_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL IfW_FFWind_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      ENDIF


         ! Update discrete states:
         !   Note that xd [discrete state] is changed in IfW_FFWind_UpdateDiscState(), so IfW_FFWind_CalcOutput(),
         !   IfW_FFWind_CalcContStateDeriv(), and IfW_FFWind_CalcConstrStates() must be called first (see above).

      CALL IfW_FFWind_UpdateDiscState(Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
      IF ( ErrStat >= AbortErrLev ) THEN
         CALL IfW_FFWind_DestroyContState( dxdt, ErrStat2, ErrMsg2)
         ErrMsg = TRIM(ErrMsg)//' '//TRIM(ErrMsg2)
         RETURN
      ENDIF


         ! Integrate (update) continuous states (x) here:

      !x = function of dxdt and x


         ! Destroy dxdt because it is not necessary for the rest of the subroutine

      CALL IfW_FFWind_DestroyContState( dxdt, ErrStat, ErrMsg)
      IF ( ErrStat >= AbortErrLev ) RETURN



END SUBROUTINE IfW_FFWind_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IfW_FFWind_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
! Tight coupling routine for computing derivatives of continuous states
!..................................................................................................................................

      REAL(DbKi),                            INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(IfW_FFWind_InputType),            INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(IfW_FFWind_ParameterType),        INTENT(IN   )  :: p           ! Parameters
      TYPE(IfW_FFWind_ContinuousStateType),  INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(IfW_FFWind_DiscreteStateType),    INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(IfW_FFWind_ConstraintStateType),  INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(IfW_FFWind_OtherStateType),       INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(IfW_FFWind_ContinuousStateType),  INTENT(  OUT)  :: dxdt        ! Continuous state derivatives at Time
      INTEGER(IntKi),                        INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                          INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Compute the first time derivatives of the continuous states here:

      dxdt%DummyContState = 0


END SUBROUTINE IfW_FFWind_CalcContStateDeriv
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IfW_FFWind_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
! Tight coupling routine for updating discrete states
!..................................................................................................................................

      REAL(DbKi),                            INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(IfW_FFWind_InputType),            INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(IfW_FFWind_ParameterType),        INTENT(IN   )  :: p           ! Parameters
      TYPE(IfW_FFWind_ContinuousStateType),  INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(IfW_FFWind_DiscreteStateType),    INTENT(INOUT)  :: xd          ! Input: Discrete states at Time;
                                                                           !   Output: Discrete states at Time + Interval
      TYPE(IfW_FFWind_ConstraintStateType),  INTENT(IN   )  :: z           ! Constraint states at Time
      TYPE(IfW_FFWind_OtherStateType),       INTENT(INOUT)  :: OtherState  ! Other/optimization states
      INTEGER(IntKi),                        INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                          INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Update discrete states here:

      ! StateData%DiscState =

END SUBROUTINE IfW_FFWind_UpdateDiscState
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE IfW_FFWind_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
! Tight coupling routine for solving for the residual of the constraint state equations
!..................................................................................................................................

      REAL(DbKi),                            INTENT(IN   )  :: Time        ! Current simulation time in seconds
      TYPE(IfW_FFWind_InputType),            INTENT(IN   )  :: u           ! Inputs at Time
      TYPE(IfW_FFWind_ParameterType),        INTENT(IN   )  :: p           ! Parameters
      TYPE(IfW_FFWind_ContinuousStateType),  INTENT(IN   )  :: x           ! Continuous states at Time
      TYPE(IfW_FFWind_DiscreteStateType),    INTENT(IN   )  :: xd          ! Discrete states at Time
      TYPE(IfW_FFWind_ConstraintStateType),  INTENT(IN   )  :: z           ! Constraint states at Time (possibly a guess)
      TYPE(IfW_FFWind_OtherStateType),       INTENT(INOUT)  :: OtherState  ! Other/optimization states
      TYPE(IfW_FFWind_ConstraintStateType),  INTENT(  OUT)  :: z_residual  ! Residual of the constraint state equations using
                                                                           ! the input values described above
      INTEGER(IntKi),                        INTENT(  OUT)  :: ErrStat     ! Error status of the operation
      CHARACTER(*),                          INTENT(  OUT)  :: ErrMsg      ! Error message if ErrStat /= ErrID_None


         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Solve for the constraint states here:

      z_residual%DummyConstrState = 0

END SUBROUTINE IfW_FFWind_CalcConstrStateResidual



!====================================================================================================
END MODULE IfW_FFWind

!  SUBROUTINE IfW_HHWind_CalcOutput(Time,    InData,        ParamData,                       &
!                             ContStates,    DiscStates,    ConstrStates,     OtherStates,   &
!                             OutData,       ErrStat,       ErrMsg)
!  SUBROUTINE IfW_HHWind_UpdateStates( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!  SUBROUTINE IfW_HHWind_CalcContStateDeriv( Time, u, p, x, xd, z, OtherState, dxdt, ErrStat, ErrMsg )
!  SUBROUTINE IfW_HHWind_UpdateDiscState( Time, u, p, x, xd, z, OtherState, ErrStat, ErrMsg )
!  SUBROUTINE IfW_HHWind_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, z_residual, ErrStat, ErrMsg )
