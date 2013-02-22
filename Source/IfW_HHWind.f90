MODULE IfW_HHWind
! This module contains all the data and procedures that define hub-height wind files. This could
! more accurately be called a point wind file since the wind speed at any point is calculated by
! shear applied to the point where wind is defined.  It is basically uniform wind over the rotor disk.
! The entire file is read on initialization, then the columns that make up the wind file are
! interpolated to the time requested, and wind is calculated based on the location in space.
!
! the file contains header information (rows that contain "!"), followed by numeric data stored in
! 8 columns:   (1) Time                                  [s]
!              (2) Horizontal wind speed       (V)       [m/s]
!              (3) Wind direction              (Delta)   [deg]
!              (4) Vertical wind speed         (VZ)      [m/s]
!              (5) Horizontal linear shear     (HLinShr) [-]
!              (6) Vertical power-law shear    (VShr)    [-]
!              (7) Vertical linear shear       (VLinShr) [-]
!              (8) Gust (horizontal) velocity  (VGust)   [m/s]
!
! The horizontal wind speed at (X, Y, Z) is then calculated using the interpolated columns by
!   Vh = V * ( Z/RefHt ) ** VShr                                        ! power-law wind shear
!      + V * HLinShr/RefWid * ( Y * COS(Delta) + X * SIN(Delta) )       ! horizontal linear shear
!      + V * VLinShr/RefWid * ( Z-RefHt )                               ! vertical linear shear
!      + VGust                                                          ! gust speed
!----------------------------------------------------------------------------------------------------

   USE                           NWTC_Library
   USE                           SharedInflowDefs
   USE                           IfW_HHWind_Types

   IMPLICIT                      NONE
   PRIVATE

   PUBLIC                       :: IfW_HHWind_Init
   PUBLIC                       :: IfW_HHWind_End
PUBLIC                       :: IfW_HHWind_GetWindSpeed        !FIXME: this is temporary
! !   PUBLIC                       :: IfW_HHWind_SetLinearizeDels       !FIXME: make this private??? or remove it??? or does the outer part go into this module???

! !   PUBLIC                       :: HH_Get_ADhack_WindSpeed                  ! REMOVE THIS!!!!

CONTAINS
!====================================================================================================
SUBROUTINE IfW_HHWind_Init(UnWind, WindFile, &  !FIXME: remove UnWind and WindFile from here.
                           InitData,   InputGuess, ParamData, &
                           ContStates, DiscStates, ConstrState,      OtherStates, &
                           OutData,    Interval,   ErrStat, ErrMsg)
! A subroutine to initialize the HHWind module.  It reads the HH file and stores the data in an
! array to use later.  It requires an initial reference height (hub height) and width (rotor diameter),
! both in meters, which are used to define the volume where wind velocities will be calculated.  This
! information is necessary because of the way the shears are defined.
!----------------------------------------------------------------------------------------------------

      ! Passed Variables: FIXME: remove these

   INTEGER,       INTENT(IN)        :: UnWind                        ! unit number for reading wind files
   CHARACTER(*),  INTENT(IN)        :: WindFile                      ! Name of the text HH wind file


      ! Passed Variables
   TYPE(IfW_HHWind_InitInputType),        INTENT(IN   )  :: InitData          ! Input data for initialization
   TYPE(IfW_HHWind_InputType),            INTENT(  OUT)  :: InputGuess        ! Initialized input data variable
   TYPE(IfW_HHWind_ParameterType),        INTENT(  OUT)  :: ParamData         ! Parameters
   TYPE(IfW_HHWind_ContinuousStateType),  INTENT(  OUT)  :: ContStates        ! Continuous States  (unused)
   TYPE(IfW_HHWind_DiscreteStateType),    INTENT(  OUT)  :: DiscStates        ! Discrete States    (unused)
   TYPE(IfW_HHWind_ConstraintStateType),  INTENT(  OUT)  :: ConstrState       ! Constraint States  (unused)
   TYPE(IfW_HHWind_OtherStateType),       INTENT(  OUT)  :: OtherStates       ! Other State data   (storage for the main data)
   TYPE(IfW_HHWind_OutputType),           INTENT(  OUT)  :: OutData           ! Initial output

   REAL(DbKi),                            INTENT(INOUT)  :: Interval          ! We don't change this.



      ! Error handling
   INTEGER,                               INTENT(  OUT)  :: ErrStat           ! determines if an error has been encountered
   CHARACTER(*),                          INTENT(  OUT)  :: ErrMsg            ! A message about the error

      ! local variables

   INTEGER,                   PARAMETER                  :: NumCols = 8       ! Number of columns in the HH file
   REAL(ReKi)                                            :: TmpData(NumCols)  ! Temp variable for reading all columns from a line
   REAL(ReKi)                                            :: DelDiff           ! Temp variable for storing the direction difference

   INTEGER                                               :: I
   INTEGER                                               :: NumComments
   INTEGER                                               :: ILine             ! Counts the line number in the file
   INTEGER,                   PARAMETER                  :: MaxTries = 100
   CHARACTER(1024)                                       :: Line              ! Temp variable for reading whole line from file

      ! Temporary variables for error handling
   INTEGER                                               :: TmpErrStat        ! Temp variable for the error status
   CHARACTER(1024)                                       :: TmpErrMsg         ! Temp variable for the error message


   !-------------------------------------------------------------------------------------------------
   ! Check that it's not already initialized
   !-------------------------------------------------------------------------------------------------

   IF ( OtherStates%TimeIndex /= 0 ) THEN
      CALL WrScr( ' HHWind has already been initialized.' )
      ErrStat = ErrId_Warn
      RETURN
   ELSE
      ErrStat = ErrId_None

      OtherStates%LinearizeDels(:)   = 0.0
      ParamData%Linearize           = .FALSE.
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Open the file for reading
   !-------------------------------------------------------------------------------------------------
   CALL OpenFInpFile (UnWind, TRIM(WindFile), ErrStat, ErrMsg)

   IF ( ErrStat /= 0 ) RETURN

   !-------------------------------------------------------------------------------------------------
   ! Find the number of comment lines
   !-------------------------------------------------------------------------------------------------
   LINE = '!'                          ! Initialize the line for the DO WHILE LOOP
   NumComments = -1

   DO WHILE (INDEX( LINE, '!' ) > 0 ) ! Lines containing "!" are treated as comment lines
      NumComments = NumComments + 1

      READ(UnWind,'( A )',IOSTAT=ErrStat) LINE

      IF ( ErrStat /=0 ) THEN
         CALL WrScr ( ' Error reading from HH wind file on line '//TRIM(Num2LStr(NumComments))//'.' )
         RETURN
      END IF

   END DO !WHILE

   !-------------------------------------------------------------------------------------------------
   ! Find the number of data lines
   !-------------------------------------------------------------------------------------------------
   OtherStates%NumDataLines = 0

   READ(LINE,*,IOSTAT=ErrStat) ( TmpData(I), I=1,NumCols )

   DO WHILE (ErrStat == 0)  ! read the rest of the file (until an error occurs)
      OtherStates%NumDataLines = OtherStates%NumDataLines + 1

      READ(UnWind,*,IOSTAT=ErrStat) ( TmpData(I), I=1,NumCols )

   END DO !WHILE


   IF (OtherStates%NumDataLines < 1) THEN
      CALL WrScr ( ' Error reading data from HH wind file on line '//TRIM(Num2LStr(OtherStates%NumDataLines+NumComments))//'.' )
      RETURN
   ELSE
      CALL WrScr ( ' Reading '//TRIM(Num2LStr(OtherStates%NumDataLines))//' lines of data from the HH wind file "'// &
                     TRIM(WindFile)//'"' )
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Allocate arrays for the HH data
   !-------------------------------------------------------------------------------------------------
   ! BJJ note: If the subroutine AllocAry() is called, the CVF compiler with A2AD does not work
   !   properly.  The arrays are not properly read even though they've been allocated.
   !-------------------------------------------------------------------------------------------------

   IF (.NOT. ALLOCATED(OtherStates%Tdata) ) THEN
      ALLOCATE ( OtherStates%Tdata(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH time array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%V) ) THEN
      ALLOCATE ( OtherStates%V(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH horizontal wind speed array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%Delta) ) THEN
      ALLOCATE ( OtherStates%Delta(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH wind direction array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%VZ) ) THEN
      ALLOCATE ( OtherStates%VZ(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical wind speed array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%HShr) ) THEN
      ALLOCATE ( OtherStates%HShr(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH horizontal linear shear array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%VShr) ) THEN
      ALLOCATE ( OtherStates%VShr(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical power-law shear exponent array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%VLinShr) ) THEN
      ALLOCATE ( OtherStates%VLinShr(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH vertical linear shear array.' )
         RETURN
      END IF
   END IF

   IF (.NOT. ALLOCATED(OtherStates%VGust) ) THEN
      ALLOCATE ( OtherStates%VGust(OtherStates%NumDataLines) , STAT=ErrStat )
      IF ( ErrStat /=0 ) THEN
         CALL WrScr( 'Error allocating memory for the HH gust velocity array.' )
         RETURN
      END IF
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Rewind the file (to the beginning) and skip the comment lines
   !-------------------------------------------------------------------------------------------------
   REWIND( UnWind )

   DO I=1,NumComments
      CALL ReadCom( UnWind, TRIM(WindFile), 'Header line #'//TRIM(Num2LStr(I)), ErrStat )
      IF ( ErrStat /= 0 ) RETURN
   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Read the data arrays
   !-------------------------------------------------------------------------------------------------

   DO I=1,OtherStates%NumDataLines

      CALL ReadAry( UnWind, TRIM(WindFile), TmpData(1:NumCols), NumCols, 'TmpData', &
                'Data from HH line '//TRIM(Num2LStr(NumComments+I)), ErrStat )
      IF (ErrStat /= 0) RETURN

      OtherStates%Tdata(  I) = TmpData(1)
      OtherStates%V(      I) = TmpData(2)
      OtherStates%Delta(  I) = TmpData(3)*D2R
      OtherStates%VZ(     I) = TmpData(4)
      OtherStates%HShr(   I) = TmpData(5)
      OtherStates%VShr(   I) = TmpData(6)
      OtherStates%VLinShr(I) = TmpData(7)
      OtherStates%VGust(  I) = TmpData(8)

   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Make sure the wind direction isn't jumping more than 180 degrees between any 2 consecutive
   ! input times.  (Avoids interpolation errors with modular arithemetic.)
   !-------------------------------------------------------------------------------------------------

   DO I=2,OtherStates%NumDataLines

      ILine = 1

      DO WHILE ( ILine < MaxTries )

         DelDiff = ( OtherStates%Delta(I) - OtherStates%Delta(I-1) )

         IF ( ABS( DelDiff ) < Pi ) EXIT  ! exit inner loop

         OtherStates%Delta(I) = OtherStates%Delta(I) - SIGN( TwoPi, DelDiff )

         ILine = ILine + 1

      END DO

      IF ( ILine >= MaxTries ) THEN
         CALL WrScr( ' Error calculating wind direction from HH file. OtherStates%Delta(' &
               // TRIM(Num2LStr(I  )) // ') = ' // TRIM(Num2LStr(OtherStates%Delta(I))) // '; OtherStates%Delta(' &
               // TRIM(Num2LStr(I+1)) // ') = ' // TRIM(Num2LStr(OtherStates%Delta(I+1))) )
         ErrStat = 1
      END IF


   END DO !I


   !-------------------------------------------------------------------------------------------------
   ! Close the file
   !-------------------------------------------------------------------------------------------------

   CLOSE( UnWind )


   !-------------------------------------------------------------------------------------------------
   ! Print warnings and messages
   !-------------------------------------------------------------------------------------------------
!   CALL WrScr ( ' Processed '//TRIM( Num2LStr( OtherStates%NumDataLines ) )//' records of HH data' )


   IF ( OtherStates%Tdata(1) > 0.0 ) THEN
      CALL ProgWarn( 'The hub-height wind file : "'//TRIM(ADJUSTL(WindFile))//'" starts at a time '// &
                     'greater than zero. Interpolation errors may result.')
   ENDIF

   IF ( OtherStates%NumDataLines == 1 ) THEN
      CALL WrScr( ' Only 1 line in HH wind file. Steady, hub-height horizontal wind speed = '// &
                  TRIM(Num2LStr(OtherStates%V(1)))//' m/s.' )
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Set the initial index into the time array (it indicates that we've initialized the module, too)
   ! and initialize the spatial scaling for the wind calculations
   !-------------------------------------------------------------------------------------------------
   OtherStates%TimeIndex = 1

   OtherStates%RefHt  = ParamData%ReferenceHeight
   OtherStates%RefWid = ParamData%Width


   RETURN

END SUBROUTINE IfW_HHWind_Init
!====================================================================================================
FUNCTION IfW_HHWind_GetWindSpeed(Time, InputPosition,                            &
                           InData,     ParamData,                                &
                           ContStates, DiscStates, ConstrState,   OtherStates,   &
                           OutData,    ErrStat,    ErrMsg)
! This subroutine linearly interpolates the columns in the HH input file to get the values for
! the requested time, then uses the interpolated values to calclate the wind speed at a point
! in space represented by InputPosition.
!----------------------------------------------------------------------------------------------------

      ! Passed Variables
   TYPE(IfW_HHWind_InputType),            INTENT(IN   )  :: InData         ! Initialized input data variable
   TYPE(IfW_HHWind_ParameterType),        INTENT(IN   )  :: ParamData      ! Parameters
   TYPE(IfW_HHWind_ContinuousStateType),  INTENT(INOUT)  :: ContStates     ! Continuous States  (unused)
   TYPE(IfW_HHWind_DiscreteStateType),    INTENT(INOUT)  :: DiscStates     ! Discrete States    (unused)
   TYPE(IfW_HHWind_ConstraintStateType),  INTENT(INOUT)  :: ConstrState    ! Constraint States  (unused)
   TYPE(IfW_HHWind_OtherStateType),       INTENT(INOUT)  :: OtherStates    ! Other State data   (storage for the main data)
   TYPE(IfW_HHWind_OutputType),           INTENT(  OUT)  :: OutData        ! Initial output


   REAL(DbKi),          INTENT(IN)  :: Time                 ! time from the start of the simulation
   REAL(ReKi),          INTENT(IN)  :: InputPosition(3)     ! input information: positions X,Y,Z
   INTEGER,             INTENT(OUT) :: ErrStat              ! error status
   CHARACTER(*),        INTENT(OUT) :: ErrMsg               ! The error message
!FIXME: make this go away when convert to subroutine
   REAL(ReKi)                       :: IfW_HHwind_GetWindSpeed(3)   ! return velocities (U,V,W)

   REAL(ReKi)                       :: CosDelta             ! cosine of Delta_tmp
   REAL(ReKi)                       :: Delta_tmp            ! interpolated Delta   at input TIME
   REAL(ReKi)                       :: HShr_tmp             ! interpolated HShr    at input TIME
   REAL(ReKi)                       :: P                    ! temporary storage for slope (in time) used in linear interpolation
   REAL(ReKi)                       :: SinDelta             ! sine of Delta_tmp
   REAL(ReKi)                       :: V_tmp                ! interpolated V       at input TIME
   REAL(ReKi)                       :: VGust_tmp            ! interpolated VGust   at input TIME
   REAL(ReKi)                       :: VLinShr_tmp          ! interpolated VLinShr at input TIME
   REAL(ReKi)                       :: VShr_tmp             ! interpolated VShr    at input TIME
   REAL(ReKi)                       :: VZ_tmp               ! interpolated VZ      at input TIME
   REAL(ReKi)                       :: V1                   ! temporary storage for horizontal velocity



   !-------------------------------------------------------------------------------------------------
   ! verify the module was initialized first
   !-------------------------------------------------------------------------------------------------

   IF ( OtherStates%TimeIndex == 0 ) THEN
      ErrMsg   = ' Error: Call HH_Init() before getting wind speed.'
      ErrStat  = ErrID_Fatal         ! Fatal since no data returned
      RETURN
   ELSE
      ErrStat = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Linearly interpolate in time (or used nearest-neighbor to extrapolate)
   ! (compare with NWTC_Num.f90\InterpStpReal)
   !-------------------------------------------------------------------------------------------------

    IF ( ParamData%Linearize ) THEN  !get the perturbed wind speed

      OtherStates%TimeIndex      = 1
      V_tmp         = OtherStates%V      (1) + OtherStates%LinearizeDels(1)
      Delta_tmp     = OtherStates%Delta  (1) + OtherStates%LinearizeDels(2)
      VZ_tmp        = OtherStates%VZ     (1) + OtherStates%LinearizeDels(3)
      HShr_tmp      = OtherStates%HShr   (1) + OtherStates%LinearizeDels(4)
      VShr_tmp      = OtherStates%VShr   (1) + OtherStates%LinearizeDels(5)
      VLinShr_tmp   = OtherStates%VLinShr(1) + OtherStates%LinearizeDels(6)
      VGust_tmp     = OtherStates%VGust  (1) + OtherStates%LinearizeDels(7)

      ! Let's check the limits.
   ELSE IF ( Time <= OtherStates%Tdata(1) .OR. OtherStates%NumDataLines == 1 )  THEN

      OtherStates%TimeIndex      = 1
      V_tmp         = OtherStates%V      (1)
      Delta_tmp     = OtherStates%Delta  (1)
      VZ_tmp        = OtherStates%VZ     (1)
      HShr_tmp      = OtherStates%HShr   (1)
      VShr_tmp      = OtherStates%VShr   (1)
      VLinShr_tmp   = OtherStates%VLinShr(1)
      VGust_tmp     = OtherStates%VGust  (1)

   ELSE IF ( Time >= OtherStates%Tdata(OtherStates%NumDataLines) )  THEN

      OtherStates%TimeIndex      = OtherStates%NumDataLines - 1
      V_tmp         = OtherStates%V      (OtherStates%NumDataLines)
      Delta_tmp     = OtherStates%Delta  (OtherStates%NumDataLines)
      VZ_tmp        = OtherStates%VZ     (OtherStates%NumDataLines)
      HShr_tmp      = OtherStates%HShr   (OtherStates%NumDataLines)
      VShr_tmp      = OtherStates%VShr   (OtherStates%NumDataLines)
      VLinShr_tmp   = OtherStates%VLinShr(OtherStates%NumDataLines)
      VGust_tmp     = OtherStates%VGust  (OtherStates%NumDataLines)

   ELSE

         ! Let's interpolate!

      OtherStates%TimeIndex = MAX( MIN( OtherStates%TimeIndex, OtherStates%NumDataLines-1 ), 1 )

      DO

         IF ( Time < OtherStates%Tdata(OtherStates%TimeIndex) )  THEN

            OtherStates%TimeIndex = OtherStates%TimeIndex - 1

         ELSE IF ( Time >= OtherStates%Tdata(OtherStates%TimeIndex+1) )  THEN

            OtherStates%TimeIndex = OtherStates%TimeIndex + 1

         ELSE
            P           = ( Time - OtherStates%Tdata(OtherStates%TimeIndex) )/( OtherStates%Tdata(OtherStates%TimeIndex+1) &
                           - OtherStates%Tdata(OtherStates%TimeIndex) )
            V_tmp       = ( OtherStates%V(      OtherStates%TimeIndex+1) - OtherStates%V(      OtherStates%TimeIndex) )*P  &
                           + OtherStates%V(      OtherStates%TimeIndex)
            Delta_tmp   = ( OtherStates%Delta(  OtherStates%TimeIndex+1) - OtherStates%Delta(  OtherStates%TimeIndex) )*P  &
                           + OtherStates%Delta(  OtherStates%TimeIndex)
            VZ_tmp      = ( OtherStates%VZ(     OtherStates%TimeIndex+1) - OtherStates%VZ(     OtherStates%TimeIndex) )*P  &
                           + OtherStates%VZ(     OtherStates%TimeIndex)
            HShr_tmp    = ( OtherStates%HShr(   OtherStates%TimeIndex+1) - OtherStates%HShr(   OtherStates%TimeIndex) )*P  &
                           + OtherStates%HShr(   OtherStates%TimeIndex)
            VShr_tmp    = ( OtherStates%VShr(   OtherStates%TimeIndex+1) - OtherStates%VShr(   OtherStates%TimeIndex) )*P  &
                           + OtherStates%VShr(   OtherStates%TimeIndex)
            VLinShr_tmp = ( OtherStates%VLinShr(OtherStates%TimeIndex+1) - OtherStates%VLinShr(OtherStates%TimeIndex) )*P  &
                           + OtherStates%VLinShr(OtherStates%TimeIndex)
            VGust_tmp   = ( OtherStates%VGust(  OtherStates%TimeIndex+1) - OtherStates%VGust(  OtherStates%TimeIndex) )*P  &
                           + OtherStates%VGust(  OtherStates%TimeIndex)
            EXIT

         END IF

      END DO

   END IF


   !-------------------------------------------------------------------------------------------------
   ! calculate the wind speed at this time
   !-------------------------------------------------------------------------------------------------

   CosDelta = COS( Delta_tmp )
   SinDelta = SIN( Delta_tmp )

   V1 = V_tmp * ( ( InputPosition(3)/OtherStates%RefHt ) ** VShr_tmp &                                  ! power-law wind shear
        + ( HShr_tmp   * ( InputPosition(2) * CosDelta + InputPosition(1) * SinDelta ) &    ! horizontal linear shear
        +  VLinShr_tmp * ( InputPosition(3)-OtherStates%RefHt ) )/OtherStates%RefWid  ) &                           ! vertical linear shear
        + VGust_tmp                                                                         ! gust speed
   IfW_HHWind_GetWindSpeed(1) =  V1 * CosDelta
   IfW_HHWind_GetWindSpeed(2) = -V1 * SinDelta
   IfW_HHWind_GetWindSpeed(3) =  VZ_tmp


   RETURN

END FUNCTION IfW_HHWind_GetWindSpeed
!====================================================================================================
SUBROUTINE IfW_HHWind_End( InData,     ParamData,                                &
                           ContStates, DiscStates, ConstrState,   OtherStates,   &
                           OutData,                                              &
                           ErrStat,    ErrMsg)

      ! Passed Variables
   TYPE(IfW_HHWind_InputType),            INTENT(INOUT)  :: InData         ! Initialized input data variable
   TYPE(IfW_HHWind_ParameterType),        INTENT(INOUT)  :: ParamData      ! Parameters
   TYPE(IfW_HHWind_ContinuousStateType),  INTENT(INOUT)  :: ContStates     ! Continuous States  (unused)
   TYPE(IfW_HHWind_DiscreteStateType),    INTENT(INOUT)  :: DiscStates     ! Discrete States    (unused)
   TYPE(IfW_HHWind_ConstraintStateType),  INTENT(INOUT)  :: ConstrState    ! Constraint States  (unused)
   TYPE(IfW_HHWind_OtherStateType),       INTENT(INOUT)  :: OtherStates    ! Other State data   (storage for the main data)
   TYPE(IfW_HHWind_OutputType),           INTENT(INOUT)  :: OutData        ! Initial output


      ! Error Handling
   INTEGER,                               INTENT(OUT)    :: ErrStat        ! determines if an error has been encountered
   CHARACTER(1024),                       INTENT(OUT)    :: ErrMsg         ! Message about errors


      ! Local Variables
   INTEGER                          :: SumErrs  !FIXME: this is depricated!!!!


      !-=- Initialize the routine -=-

   SumErrs = 0

   IF ( ALLOCATED(OtherStates%Tdata  ) ) DEALLOCATE( OtherStates%Tdata,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%Delta  ) ) DEALLOCATE( OtherStates%Delta,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%V      ) ) DEALLOCATE( OtherStates%V,       STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%VZ     ) ) DEALLOCATE( OtherStates%VZ,      STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%HShr   ) ) DEALLOCATE( OtherStates%HShr,    STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%VShr   ) ) DEALLOCATE( OtherStates%VShr,    STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%VLinShr) ) DEALLOCATE( OtherStates%VLinShr, STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   IF ( ALLOCATED(OtherStates%VGust  ) ) DEALLOCATE( OtherStates%VGust,   STAT=ErrStat )
   SumErrs = SumErrs + ABS(ErrStat)

   ErrStat  = SumErrs
   OtherStates%TimeIndex = 0

END SUBROUTINE IfW_HHWind_End
!====================================================================================================


!====================================================================================================
!====================================================================================================
!REMOVED:
!!====================================================================================================
!FUNCTION HH_Get_ADHack_WindSpeed(Time, InputPosition, ErrStat, ErrMsg)
!! This subroutine linearly interpolates the columns in the HH input file to get the values for
!! the requested time, then uses the interpolated values to calclate the wind speed at a point
!! in space represented by InputPosition. THIS FUNCTION SHOULD BE REMOVED!!!!! (used for DISK VEL ONLY)
!!----------------------------------------------------------------------------------------------------
!
!   REAL(ReKi),          INTENT(IN)  :: Time                 ! time from the start of the simulation
!   REAL(ReKi),          INTENT(IN)  :: InputPosition(3)     ! input information: positions X,Y,Z   -   NOT USED HERE!!!
!   INTEGER,             INTENT(OUT) :: ErrStat              ! error status
!   CHARACTER(*),        INTENT(OUT) :: ErrMsg               ! The error message
!   REAL(ReKi)                       :: HH_Get_ADHack_WindSpeed(3)      ! return velocities (U,V,W)
!
!   REAL(ReKi)                       :: Delta_tmp            ! interpolated Delta   at input TIME
!   REAL(ReKi)                       :: P                    ! temporary storage for slope (in time) used in linear interpolation
!   REAL(ReKi)                       :: V_tmp                ! interpolated V       at input TIME
!   REAL(ReKi)                       :: VZ_tmp               ! interpolated VZ      at input TIME
!
!
!   !-------------------------------------------------------------------------------------------------
!   ! verify the module was initialized first
!   !-------------------------------------------------------------------------------------------------
!
!   IF ( TimeIndex == 0 ) THEN
!      ErrMsg   = ' Error: Call HH_Init() before getting wind speed.'
!      ErrStat  = ErrID_Fatal      ! Fatal since no data returned
!      RETURN
!   ELSE
!      ErrStat = 0
!   END IF
!
!
!   !-------------------------------------------------------------------------------------------------
!   ! Linearly interpolate in time (or use nearest-neighbor to extrapolate)
!   ! (compare with NWTC_Num.f90\InterpStpReal)
!   !-------------------------------------------------------------------------------------------------
!
!     ! Let's check the limits.
!
!   IF ( Time <= OtherStates%Tdata(1) .OR. OtherStates%NumDataLines == 1)  THEN
!
!      TimeIndex      = 1
!      V_tmp         = V      (1)
!      Delta_tmp     = Delta  (1)
!      VZ_tmp        = VZ     (1)
!
!   ELSE IF ( Time >= OtherStates%Tdata(OtherStates%NumDataLines) )  THEN
!
!      TimeIndex      = OtherStates%NumDataLines - 1
!      V_tmp         = V      (OtherStates%NumDataLines)
!      Delta_tmp     = Delta  (OtherStates%NumDataLines)
!      VZ_tmp        = VZ     (OtherStates%NumDataLines)
!
!   ELSE
!
!         ! Let's interpolate!
!
!      TimeIndex = MAX( MIN( TimeIndex, OtherStates%NumDataLines-1 ), 1 )
!
!      DO
!
!         IF ( Time < OtherStates%Tdata(TimeIndex) )  THEN
!
!            TimeIndex = TimeIndex - 1
!
!         ELSE IF ( Time >= OtherStates%Tdata(TimeIndex+1) )  THEN
!
!            TimeIndex = TimeIndex + 1
!
!         ELSE
!            P           = ( Time - OtherStates%Tdata(TimeIndex) )/( OtherStates%Tdata(TimeIndex+1) - OtherStates%Tdata(TimeIndex) )
!            V_tmp       = ( V(      TimeIndex+1) - V(      TimeIndex) )*P + V(      TimeIndex)
!            Delta_tmp   = ( Delta(  TimeIndex+1) - Delta(  TimeIndex) )*P + Delta(  TimeIndex)
!            VZ_tmp      = ( VZ(     TimeIndex+1) - VZ(     TimeIndex) )*P + VZ(     TimeIndex)
!            EXIT
!
!         END IF
!
!      END DO
!
!   END IF
!
!   !-------------------------------------------------------------------------------------------------
!   ! calculate the wind speed at this time
!   !-------------------------------------------------------------------------------------------------
!   HH_Get_ADHack_WindSpeed(1) =  V_tmp * COS( Delta_tmp )
!   HH_Get_ADHack_WindSpeed(2) = -V_tmp * SIN( Delta_tmp )
!   HH_Get_ADHack_WindSpeed(3) =  VZ_tmp
!
!
!   RETURN
!
!END FUNCTION HH_Get_ADHack_WindSpeed
!----------------------------------------------------------------------------------------------------




!====================================================================================================
!====================================================================================================
!====================================================================================================
! !!MOVED FROM ABOVE:
! !!----------------------------------------------------------------------------------------------------
! !!FIXME: might need to move this into states???
! !SUBROUTINE IfW_HHWind_SetLinearizeDels( Perturbations, ErrStat, ErrMsg )
! !! This subroutine sets the perturbation values for the linearization scheme.
! !
! !   REAL(ReKi),          INTENT(IN)  :: Perturbations(7)     ! purturbations for each of the 7 input parameters
! !   INTEGER,             INTENT(OUT) :: ErrStat              ! time from the start of the simulation
! !   CHARACTER(*),        INTENT(OUT) :: ErrMsg               ! Error Message
! !
! !   !-------------------------------------------------------------------------------------------------
! !   ! verify the module was initialized first
! !   !-------------------------------------------------------------------------------------------------
! !
! !   IF ( TimeIndex == 0 ) THEN
! !      ErrMsg   = ' Error: Call HH_Init() before getting wind speed.'
! !      ErrStat  = ErrID_Fatal        ! Fatal since no data returned
! !      RETURN
! !   ELSE
! !      ErrStat = 0
! !   END IF
! !
! !   ParamData%Linearize = .TRUE.
! !   OtherStates%LinearizeDels(:) = Perturbations(:)
! !
! !   RETURN
! !
! !END SUBROUTINE IfW_HHWind_SetLinearizeDels
! !!====================================================================================================
END MODULE IfW_HHWind
