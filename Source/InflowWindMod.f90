MODULE InflowWind
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


   IMPLICIT                         NONE
   PRIVATE

   !-------------------------------------------------------------------------------------------------
   ! Private internal variables
   !-------------------------------------------------------------------------------------------------

!FIXME: handle this differently -- should be allocated by the library function to get an open unit number
! store as parameter in parametertype
   INTEGER                        :: UnWind   = 91          ! The unit number used for wind inflow files


   !-------------------------------------------------------------------------------------------------
   ! Definitions of public types and routines
   !-------------------------------------------------------------------------------------------------
!FIXME: move to types
   TYPE, PUBLIC :: InflInitInfo
      CHARACTER(1024)             :: WindFileName
      INTEGER                     :: WindFileType
      REAL(ReKi)                  :: ReferenceHeight        ! reference height for HH and/or 4D winds (was hub height), in meters
      REAL(ReKi)                  :: Width                  ! width of the HH file (was 2*R), in meters
   END TYPE InflInitInfo

   PUBLIC                         :: InflowWind_Init           ! Initialization subroutine

!FIXME: should not be public anymore
   PUBLIC                         :: InflowWind_GetVelocity    ! function to get wind speed at point in space and time

   PUBLIC                         :: InflowWind_Terminate      ! subroutine to clean up

!FIXME: not public anymore. may not exist when done.
!   PUBLIC                         :: InflowWind_ADhack_diskVel ! used to keep old AeroDyn functionality--remove soon!
!   PUBLIC                         :: InflowWind_ADhack_DIcheck ! used to keep old AeroDyn functionality--remove soon!

!FIXME: not public anymore.
!   PUBLIC                         :: InflowWind_LinearizePerturbation !used for linearization; should be modified
!!----Removed during conversion to new framework: may put back in as part of OtherStates
!!       PUBLIC                         :: InflowWind_GetMean        ! function to get the mean wind speed at a point in space
!!       PUBLIC                         :: InflowWind_GetStdDev      ! function to calculate standard deviation at a point in space
!!       PUBLIC                         :: InflowWind_GetTI          ! function to get TI at a point in space

!FIXME: tie this to a type as well. Change to ProgDesc type
   CHARACTER(99),PARAMETER        :: InflowWindVer = 'InflowWind (v1.01.00b-bjj, 10-Aug-2012)'

CONTAINS
!====================================================================================================
!FIXME: change naming to new standard. InflowWind_Init or IfW_Init
SUBROUTINE InflowWind_Init( FileInfo, ErrStat )
!  Open and read the wind files, allocating space for necessary variables
!
!----------------------------------------------------------------------------------------------------

      ! Passed variables

   TYPE(InflInitInfo), INTENT(IN)   :: FileInfo
   INTEGER,            INTENT(OUT)  :: ErrStat

      ! Local variables

   TYPE(HH_Info)                    :: HHInitInfo
   TYPE(CT_Backgr)                  :: BackGrndValues

   REAL(ReKi)                       :: Height
   REAL(ReKi)                       :: HalfWidth
   CHARACTER(1024)                  :: FileName

!FIXME: not sure if want to track it this way or not.
   IF ( WindType /= Undef_Wind ) THEN
      CALL WrScr( ' Wind inflow has already been initialized.' )
      ErrStat = 1
      RETURN
   ELSE
      WindType = FileInfo%WindFileType
      FileName = FileInfo%WindFileName
      CALL NWTC_Init()
      CALL WrScr1( ' Using '//TRIM( InflowWindVer ) )

   END IF

   !-------------------------------------------------------------------------------------------------
   ! Get default wind type, based on file name, if requested
   !-------------------------------------------------------------------------------------------------
   IF ( FileInfo%WindFileType == DEFAULT_Wind ) THEN
      WindType = GetWindType( FileName, ErrStat )
   END IF


   !-------------------------------------------------------------------------------------------------
   ! Check for coherent turbulence file (KH superimposed on a background wind file)
   ! Initialize the CTWind module and initialize the module of the other wind type.
   !-------------------------------------------------------------------------------------------------

   IF ( WindType == CTP_Wind ) THEN

      CALL CT_Init(UnWind, FileName, BackGrndValues, ErrStat)
      IF (ErrStat /= 0) THEN
         CALL InflowWind_Terminate( ErrStat )
         WindType = Undef_Wind
         ErrStat  = 1
         RETURN
      END IF

      FileName = BackGrndValues%WindFile
      WindType = BackGrndValues%WindFileType
      CT_Flag  = BackGrndValues%CoherentStr

   ELSE

      CT_Flag  = .FALSE.

   END IF


   !-------------------------------------------------------------------------------------------------
   ! Initialize based on the wind type
   !-------------------------------------------------------------------------------------------------

   SELECT CASE ( WindType )

      CASE (HH_Wind)

         HHInitInfo%ReferenceHeight = FileInfo%ReferenceHeight
         HHInitInfo%Width           = FileInfo%Width

         CALL HH_Init( UnWind, FileName, HHInitInfo, ErrStat )

!        IF (CT_Flag) CALL CT_SetRefVal(FileInfo%ReferenceHeight, 0.5*FileInfo%Width, ErrStat)
         IF (ErrStat == 0 .AND. CT_Flag) CALL CT_SetRefVal(FileInfo%ReferenceHeight, REAL(0.0, ReKi), ErrStat)


      CASE (FF_Wind)

         CALL FF_Init( UnWind, FileName, ErrStat )


            ! Set CT parameters

         IF ( ErrStat == 0 .AND. CT_Flag ) THEN
            Height     = FF_GetValue('HubHeight', ErrStat)
            IF ( ErrStat /= 0 ) Height = FileInfo%ReferenceHeight

            HalfWidth  = 0.5*FF_GetValue('GridWidth', ErrStat)
            IF ( ErrStat /= 0 ) HalfWidth = 0

            CALL CT_SetRefVal(Height, HalfWidth, ErrStat)
         END IF


      CASE (UD_Wind)

         CALL UsrWnd_Init(ErrStat)


      CASE (FD_Wind)

         CALL FD_Init(UnWind, FileName, FileInfo%ReferenceHeight, ErrStat)

      CASE (HAWC_Wind)

         CALL HW_Init( UnWind, FileName, ErrStat )

      CASE DEFAULT

         CALL WrScr(' Error: Undefined wind type in WindInflow_Init()' )
         ErrStat = 1
         RETURN

   END SELECT

   IF ( ErrStat /= 0 ) THEN
      CALL InflowWind_Terminate( ErrStat )  !Just in case we've allocated something
      WindType = Undef_Wind
      ErrStat  = 1
   END IF

   RETURN

END SUBROUTINE InflowWind_Init
!====================================================================================================
!FIXME: rename as per framework.
SUBROUTINE InflowWind_Terminate( ErrStat )
! Clean up the allocated variables and close all open files.  Reset the initialization flag so
! that we have to reinitialize before calling the routines again.
!----------------------------------------------------------------------------------------------------

   INTEGER, INTENT(OUT)       :: ErrStat     !bjj: do we care if there's an error on cleanup?


      ! Close the wind file, if it happens to be open

   CLOSE( UnWind )


      ! End the sub-modules (deallocates their arrays and closes their files):

   SELECT CASE ( WindType )

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
         CALL WrScr(' Undefined wind type in InflowWind_Terminate().' )
         ErrStat = 1

   END SELECT

!   IF (CT_Flag) CALL CT_Terminate( ErrStat )
   CALL CT_Terminate( ErrStat )


      ! Reset the wind type so that the initialization routine must be called
  WindType = Undef_Wind
   CT_Flag  = .FALSE.


END SUBROUTINE InflowWind_Terminate
!====================================================================================================
END MODULE InflowWind



