MODULE InflowWind_Subs
! This module is used to read and process the (undisturbed) inflow winds.  It must be initialized
! using InflowWind_Init() with the name of the file, the file type, and possibly reference height and
! width (depending on the type of wind file being used).  This module calls appropriate routines
! in the wind modules so that the type of wind becomes seamless to the user.  InflowWind_Terminate()
! should be called when the program has finshed.
!
! Data are assumed to be in units of meters and seconds.  Z is measured from the ground (NOT the hub!).
!
!  7 Oct 2009    Initial Release with AeroDyn 13.00.00      B. Jonkman, NREL/NWTC
! 14 Nov 2011    v1.00.01b-bjj                              B. Jonkman
!  1 Aug 2012    v1.01.00a-bjj                              B. Jonkman
! 10 Aug 2012    v1.01.00b-bjj                              B. Jonkman
!----------------------------------------------------------------------------------------------------

   USE                              NWTC_Library
   USE                              SharedInflowDefs
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


   IMPLICIT                         NONE



CONTAINS
!====================================================================================================
SUBROUTINE GetWindType( ParamData, ErrStat, ErrMsg )
!  This subroutine checks the file FileName to see what kind of wind file we are using.  Used when
!  the wind file type is unknown.
!----------------------------------------------------------------------------------------------------
!FIXME: may want to change this to a subroutine that sets stuff in the passed IfW_ParameterType variable

   IMPLICIT             NONE


      ! Passed Variables:

   TYPE( IfW_ParameterType),        INTENT(INOUT)     :: ParamData
   INTEGER(IntKi),                  INTENT(  OUT)     :: ErrStat
   CHARACTER(*),                    INTENT(  OUT)     :: ErrMsg


      ! Local Variables:

   INTEGER                                            :: IND
   LOGICAL                                            :: Exists

   CHARACTER(1024)                                    :: FileName       ! Temporary name holder
   CHARACTER(  3)                                     :: FileNameEnd
   CHARACTER(  8)                                     :: WndFilNam      ! Temporary name holder

   CHARACTER(1024)                                    :: FileRoot


   ErrStat  = 0
   ErrMsg   = ""

   !-------------------------------------------------------------------------------------------------
   ! Check for user-defined wind file first; file starts with "USERWIND"
   !-------------------------------------------------------------------------------------------------

   WndFilNam = ParamData%WindFileName
   FileName  = ParamData%WindFileName
   CALL Conv2UC( WndFilNam )              ! convert name to upper case

   IF ( WndFilNam == 'USERWIND' )  THEN

      CALL WrScr1( ' Detected user-defined wind file.' )
      ParamData%WindFileType = UD_Wind

      RETURN
   END IF

   !-------------------------------------------------------------------------------------------------
   ! Get the file extension (or at least what we expect the extension to be)
   !-------------------------------------------------------------------------------------------------
   CALL GetRoot ( ParamData%WindFileName, ParamData%WindFileNameRoot )            ! Get the root name

   IND = LEN_TRIM( ParamData%WindFileNameRoot ) + 1
   IF ( IND < LEN_TRIM( ParamData%WindFileName ) ) THEN
         ! Get the extention, starting at first character past (may not be the whole "extension")
      FileNameEnd = ParamData%WindFileName(IND+1:)
      CALL Conv2UC (FileNameEnd)
   ELSE
      FileNameEnd = ""
      IND = 0
   END IF


   !-------------------------------------------------------------------------------------------------
   ! If there was no '.' in the file name, assume FF, and add a .wnd extension
   !-------------------------------------------------------------------------------------------------
   IF ( IND == 0 ) THEN
      CALL WrScr1(' No file extension found. Assuming '//TRIM(FileName)// &
                  ' is a binary FF wind file with a ".wnd" extension.')
      ParamData%WindFileType = FF_Wind
      FileName = TRIM(FileName)//'.wnd'
      RETURN
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Base the file type on the extension
   !-------------------------------------------------------------------------------------------------
   SELECT CASE ( TRIM(FileNameEnd) )
      CASE ('WND')

            ! If a summary file exists, assume FF; otherwise, assume HH file.

         INQUIRE ( FILE=FileName(1:IND)//'sum' , EXIST=Exists )
         IF (Exists) THEN
            CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary FF wind file.')
            ParamData%WindFileType = FF_Wind
         ELSE
            CALL WrScr1(' Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
            ParamData%WindFileType = HH_Wind
         END IF

      CASE ('BTS')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary FF wind file.')
         ParamData%WindFileType = FF_Wind

      CASE ('CTP')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a coherent turbulence wind file.')
         ParamData%WindFileType = CTP_Wind

      CASE ('FDP')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a binary 4-dimensional wind file.')
         ParamData%WindFileType = FD_Wind

      CASE ('HWC')
         CALL WrScr1(' Assuming '//TRIM(FileName)//' contains full-field wind parameters in HAWC format.')
         ParamData%WindFileType = HAWC_Wind

      CASE DEFAULT
         CALL WrScr1(' Assuming '//TRIM(FileName)//' is a formatted HH wind file.')
         ParamData%WindFileType = HH_Wind

   END SELECT


RETURN
END SUBROUTINE GetWindType
!====================================================================================================
SUBROUTINE InflowWind_LinearizePerturbation( ParamData, LinPerturbations, ErrStat )
! This function is used in FAST's linearization scheme.  It should be fixed at some point.
!----------------------------------------------------------------------------------------------------

      ! Passed variables

   TYPE( IfW_ParameterType),        INTENT(INOUT)     :: ParamData

   INTEGER,                         INTENT(OUT)       :: ErrStat

   REAL(ReKi),                      INTENT(IN)        :: LinPerturbations(7)



      ! Local variables


   ErrStat = 0

   SELECT CASE ( ParamData%WindFileType )
      CASE (HH_Wind)

         CALL HH_SetLinearizeDels( LinPerturbations, ErrStat )

      CASE ( FF_Wind, UD_Wind, FD_Wind, HAWC_Wind )

         CALL WrScr( ' Error: Linearization is valid only with HH wind files.' )
         ErrStat = 1

      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in InflowWind_LinearizePerturbation(). '// &
                     'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1

   END SELECT


END SUBROUTINE InflowWind_LinearizePerturbation
!! FIXME: This has been removed for now. I don't know what will happen to this after the conversion to the framework. Might still be needed at that point.
!! !====================================================================================================
!! FUNCTION InflowWind_ADhack_diskVel( Time, InpPosition, ErrStat )
!! ! This function should be deleted ASAP.  It's purpose is to reproduce results of AeroDyn 12.57;
!! ! when a consensus on the definition of "average velocity" is determined, this function will be
!! ! removed.  InpPosition(2) should be the rotor radius; InpPosition(3) should be hub height
!! !----------------------------------------------------------------------------------------------------
!! 
!!       ! Passed variables
!! 
!!    REAL(ReKi), INTENT(IN)     :: Time
!!    REAL(ReKi), INTENT(IN)     :: InpPosition(3)
!!    INTEGER, INTENT(OUT)       :: ErrStat
!! 
!!       ! Function definition
!!    REAL(ReKi)                 :: InflowWind_ADhack_diskVel(3)
!! 
!!       ! Local variables
!!    TYPE(InflIntrpOut)         :: NewVelocity             ! U, V, W velocities
!!    REAL(ReKi)                 :: Position(3)
!!    INTEGER                    :: IY
!!    INTEGER                    :: IZ
!! 
!! 
!!    ErrStat = 0
!! 
!!    SELECT CASE ( ParamData%WindFileType )
!!       CASE (HH_Wind)
!! 
!! !      VXGBAR =  V * COS( DELTA )
!! !      VYGBAR = -V * SIN( DELTA )
!! !      VZGBAR =  VZ
!! 
!!          Position    = (/ REAL(0.0, ReKi), REAL(0.0, ReKi), InpPosition(3) /)
!!          NewVelocity = HH_Get_ADHack_WindSpeed(Time, Position, ErrStat)
!! 
!!          InflowWind_ADhack_diskVel(:) = NewVelocity%Velocity(:)
!! 
!! 
!!       CASE (FF_Wind)
!! !      VXGBAR = MeanFFWS
!! !      VYGBAR = 0.0
!! !      VZGBAR = 0.0
!! 
!!          InflowWind_ADhack_diskVel(1)   = FF_GetValue('MEANFFWS', ErrStat)
!!          InflowWind_ADhack_diskVel(2:3) = 0.0
!! 
!!       CASE (UD_Wind)
!! !      VXGBAR = UWmeanU
!! !      VYGBAR = UWmeanV
!! !      VZGBAR = UWmeanW
!! 
!!          InflowWind_ADhack_diskVel(1)   = UsrWnd_GetValue('MEANU', ErrStat)
!!          IF (ErrStat /= 0) RETURN
!!          InflowWind_ADhack_diskVel(2)   = UsrWnd_GetValue('MEANV', ErrStat)
!!          IF (ErrStat /= 0) RETURN
!!          InflowWind_ADhack_diskVel(3)   = UsrWnd_GetValue('MEANW', ErrStat)
!! 
!!       CASE (FD_Wind)
!! !      XGrnd = 0.0
!! !      YGrnd = 0.5*RotDiam
!! !      ZGrnd = 0.5*RotDiam
!! !      CALL FD_Interp
!! !      VXGBAR = FDWind( 1 )
!! !      VYGBAR = FDWind( 2 )
!! !      VZGBAR = FDWind( 3 )
!! !
!! !      XGrnd =  0.0
!! !      YGrnd = -0.5*RotDiam
!! !      ZGrnd =  0.5*RotDiam
!! !      CALL FD_Interp
!! !      VXGBAR = VXGBAR + FDWind( 1 )
!! !      VYGBAR = VYGBAR + FDWind( 2 )
!! !      VZGBAR = VZGBAR + FDWind( 3 )
!! !
!! !      XGrnd =  0.0
!! !      YGrnd = -0.5*RotDiam
!! !      ZGrnd = -0.5*RotDiam
!! !      CALL FD_Interp
!! !      VXGBAR = VXGBAR + FDWind( 1 )
!! !      VYGBAR = VYGBAR + FDWind( 2 )
!! !      VZGBAR = VZGBAR + FDWind( 3 )
!! !
!! !      XGrnd =  0.0
!! !      YGrnd =  0.5*RotDiam
!! !      ZGrnd = -0.5*RotDiam
!! !      CALL FD_Interp
!! !      VXGBAR = 0.25*( VXGBAR + FDWind( 1 ) )
!! !      VYGBAR = 0.25*( VYGBAR + FDWind( 2 ) )
!! !      VZGBAR = 0.25*( VZGBAR + FDWind( 3 ) )
!! 
!! 
!!          Position(1) = 0.0
!!          InflowWind_ADhack_diskVel(:) = 0.0
!! 
!!          DO IY = -1,1,2
!!             Position(2)  =  IY*FD_GetValue('RotDiam',ErrStat)
!! 
!!             DO IZ = -1,1,2
!!                Position(3)  = IZ*InpPosition(2) + InpPosition(3)
!! 
!!                NewVelocity = InflowWind_GetVelocity(Time, Position, ErrStat)
!!                InflowWind_ADhack_diskVel(:) = InflowWind_ADhack_diskVel(:) + NewVelocity%Velocity(:)
!!             END DO
!!          END DO
!!          InflowWind_ADhack_diskVel(:) = 0.25*InflowWind_ADhack_diskVel(:)
!! 
!!       CASE (HAWC_Wind)
!!          InflowWind_ADhack_diskVel(1)   = HW_GetValue('UREF', ErrStat)
!!          InflowWind_ADhack_diskVel(2:3) = 0.0
!! 
!!       CASE DEFAULT
!!          CALL WrScr(' Error: Undefined wind type in InflowWind_ADhack_diskVel(). '// &
!!                     'Call WindInflow_Init() before calling this function.' )
!!          ErrStat = 1
!! 
!!    END SELECT
!! 
!!    RETURN
!! 
!! END FUNCTION InflowWind_ADhack_diskVel
!====================================================================================================
FUNCTION InflowWind_ADhack_DIcheck( ParamData, ErrStat )
! This function should be deleted ASAP.  It's purpose is to reproduce results of AeroDyn 12.57;
! it performs a wind speed check for the dynamic inflow initialization
! it returns MFFWS for the FF wind files; for all others, a sufficiently large number is used ( > 8 m/s)
!----------------------------------------------------------------------------------------------------

      ! Passed variables

   TYPE( IfW_ParameterType),        INTENT(INOUT)     :: ParamData

   INTEGER,                         INTENT(OUT)       :: ErrStat

      ! Function definition
   REAL(ReKi)                 :: InflowWind_ADhack_DIcheck


   ErrStat = 0

   SELECT CASE ( ParamData%WindFileType )
      CASE (HH_Wind, UD_Wind, FD_Wind )

         InflowWind_ADhack_DIcheck = 50  ! just return something greater than 8 m/s

      CASE (FF_Wind)

         InflowWind_ADhack_DIcheck = FF_GetValue('MEANFFWS', ErrStat)

      CASE (HAWC_Wind)

         InflowWind_ADhack_DIcheck = HW_GetValue('UREF', ErrStat)

      CASE DEFAULT
         CALL WrScr(' Error: Undefined wind type in InflowWind_ADhack_DIcheck(). '// &
                    'Call WindInflow_Init() before calling this function.' )
         ErrStat = 1

   END SELECT

   RETURN

END FUNCTION InflowWind_ADhack_DIcheck
!====================================================================================================
END MODULE InflowWind_Subs





!!----Remove this functionality for now. Might put it back in sometime after the conversion to the new framework ----
!!    FUNCTION InflowWind_GetMean(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!!    !  This function returns the mean wind speed
!!    !----------------------------------------------------------------------------------------------------
!!
!!          ! passed variables
!!       REAL(ReKi),       INTENT(IN)  :: StartTime
!!       REAL(ReKi),       INTENT(IN)  :: EndTime
!!       REAL(ReKi),       INTENT(IN)  :: delta_time
!!       REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
!!       INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
!!
!!          ! function definition
!!       REAL(ReKi)                    :: InflowWind_GetMean(3)      ! MEAN U, V, W
!!
!!          ! local variables
!!       REAL(ReKi)                    :: Time
!!       REAL(DbKi)                    :: SumVel(3)
!!       INTEGER                       :: I
!!       INTEGER                       :: Nt
!!
!!       TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
!!
!!
!!       Nt = (EndTime - StartTime) / delta_time
!!
!!       SumVel(:) = 0.0
!!       ErrStat   = 0
!!
!!
!!       DO I=1,Nt
!!
!!          Time = StartTime + (I-1)*delta_time
!!
!!          NewVelocity = InflowWind_GetVelocity(Time, InputPosition, ErrStat)
!!          IF ( ErrStat /= 0 ) THEN
!!             InflowWind_GetMean(:) = SumVel(:) / REAL(I-1, ReKi)
!!             RETURN
!!          ELSE
!!             SumVel(:) = SumVel(:) + NewVelocity%Velocity(:)
!!          END IF
!!
!!       END DO
!!
!!       InflowWind_GetMean(:) = SumVel(:) / REAL(Nt, ReKi)
!!
!!
!!    END FUNCTION InflowWind_GetMean
!!    !====================================================================================================
!!    FUNCTION InflowWind_GetStdDev(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!!    !  This function returns the mean wind speed (mean, std, TI, etc)
!!    !----------------------------------------------------------------------------------------------------
!!
!!          ! passed variables
!!       REAL(ReKi),       INTENT(IN)  :: StartTime
!!       REAL(ReKi),       INTENT(IN)  :: EndTime
!!       REAL(ReKi),       INTENT(IN)  :: delta_time
!!       REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
!!       INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
!!
!!          ! function definition
!!       REAL(ReKi)                    :: InflowWind_GetStdDev(3)    ! STD U, V, W
!!
!!          ! local variables
!!       REAL(ReKi)                    :: Time
!!       REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
!!       REAL(DbKi)                    :: SumAry(3)
!!       REAL(DbKi)                    :: MeanVel(3)
!!       INTEGER                       :: I
!!       INTEGER                       :: Nt
!!
!!       TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Initialize
!!       !-------------------------------------------------------------------------------------------------
!!
!!       InflowWind_GetStdDev(:) = 0.0
!!
!!       Nt = (EndTime - StartTime) / delta_time
!!
!!       IF ( Nt < 2 ) RETURN    ! StdDev is 0
!!
!!
!!       IF (.NOT. ALLOCATED(Velocity)) THEN
!!    !      CALL AllocAry( Velocity, 3, Nt, 'StdDev velocity', ErrStat)
!!          ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )
!!
!!          IF ( ErrStat /= 0 )  THEN
!!             CALL WrScr ( ' Error allocating memory for the StdDev velocity array.' )
!!             RETURN
!!          END IF
!!       END IF
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the mean, storing the velocity for later
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          Time = StartTime + (I-1)*delta_time
!!
!!          NewVelocity = InflowWind_GetVelocity(Time, InputPosition, ErrStat)
!!          IF ( ErrStat /= 0 ) RETURN
!!          Velocity(:,I) = NewVelocity%Velocity(:)
!!          SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
!!
!!       END DO
!!
!!       MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the standard deviation
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
!!
!!       END DO ! I
!!
!!       InflowWind_GetStdDev(:) = SQRT( SumAry(:) / ( Nt - 1 ) )
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Deallocate
!!       !-------------------------------------------------------------------------------------------------
!!       IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
!!
!!
!!    END FUNCTION InflowWind_GetStdDev
!!    !====================================================================================================
!!    FUNCTION InflowWind_GetTI(StartTime, EndTime, delta_time, InputPosition,  ErrStat )
!!    !  This function returns the TI of the wind speed.  It's basically a copy of InflowWind_GetStdDev,
!!    !  except the return value is divided by the mean U-component wind speed.
!!    !----------------------------------------------------------------------------------------------------
!!
!!          ! passed variables
!!       REAL(ReKi),       INTENT(IN)  :: StartTime
!!       REAL(ReKi),       INTENT(IN)  :: EndTime
!!       REAL(ReKi),       INTENT(IN)  :: delta_time
!!       REAL(ReKi),       INTENT(IN)  :: InputPosition(3)        ! X, Y, Z positions
!!       INTEGER,          INTENT(OUT) :: ErrStat                 ! Return 0 if no error; non-zero otherwise
!!
!!          ! function definition
!!       REAL(ReKi)                    :: InflowWind_GetTI(3)        ! TI U, V, W
!!
!!          ! local variables
!!       REAL(ReKi)                    :: Time
!!       REAL(ReKi), ALLOCATABLE       :: Velocity(:,:)
!!       REAL(DbKi)                    :: SumAry(3)
!!       REAL(DbKi)                    :: MeanVel(3)
!!       INTEGER                       :: I
!!       INTEGER                       :: Nt
!!
!!       TYPE(InflIntrpOut)            :: NewVelocity             ! U, V, W velocities
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Initialize
!!       !-------------------------------------------------------------------------------------------------
!!
!!       InflowWind_GetTI(:) = 0.0
!!
!!       Nt = (EndTime - StartTime) / delta_time
!!
!!       IF ( Nt < 2 ) RETURN    ! StdDev is 0
!!
!!
!!       IF (.NOT. ALLOCATED(Velocity)) THEN
!!    !      CALL AllocAry( Velocity, 3, Nt, 'TI velocity', ErrStat)
!!          ALLOCATE ( Velocity(3, Nt), STAT=ErrStat )
!!
!!          IF ( ErrStat /= 0 )  THEN
!!             CALL WrScr ( ' Error allocating memory for the TI velocity array.' )
!!             RETURN
!!          END IF
!!       END IF
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the mean, storing the velocity for later
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          Time = StartTime + (I-1)*delta_time
!!
!!          NewVelocity = InflowWind_GetVelocity(Time, InputPosition, ErrStat)
!!          IF ( ErrStat /= 0 ) RETURN
!!          Velocity(:,I) = NewVelocity%Velocity(:)
!!          SumAry(:)     = SumAry(:) + NewVelocity%Velocity(:)
!!
!!       END DO
!!
!!       MeanVel(:) = SumAry(:) / REAL(Nt, ReKi)
!!
!!       IF ( ABS(MeanVel(1)) <= EPSILON(MeanVel(1)) ) THEN
!!          CALL WrScr( ' Wind speed is small in InflowWind_GetTI(). TI is undefined.' )
!!          ErrStat = 1
!!          RETURN
!!       END IF
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Calculate the standard deviation
!!       !-------------------------------------------------------------------------------------------------
!!       SumAry(:) = 0.0
!!
!!       DO I=1,Nt
!!
!!          SumAry(:) = SumAry(:) + ( Velocity(:,I) - MeanVel(:) )**2
!!
!!       END DO ! I
!!
!!       InflowWind_GetTI(:) = SQRT( SumAry(:) / ( Nt - 1 ) ) / MeanVel(1)
!!
!!
!!       !-------------------------------------------------------------------------------------------------
!!       ! Deallocate
!!       !-------------------------------------------------------------------------------------------------
!!       IF ( ALLOCATED(Velocity) ) DEALLOCATE( Velocity )
!!
!!
!!    END FUNCTION InflowWind_GetTI
