!**********************************************************************************************************************************
!
!  MODULE: IfW_Driver_Subs  - This module contains subroutines used by the InflowWind Driver program
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
MODULE Ifw_Driver_Subs

   USE NWTC_Library
   IMPLICIT NONE


CONTAINS
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
SUBROUTINE DispHelpText( ErrStat, ErrMsg )
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!
      ! Print out help information  !
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!

   USE NWTC_Library

   IMPLICIT NONE

      ! Error Handling
   INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat
   CHARACTER(1024),                    INTENT(  OUT)  :: ErrMsg


      !  Statement about usage
   CALL WrScr("  Syntax:  InlowWind_Driver <filename> [options]")
   CALL WrScr("")
   CALL WrScr("       where:     <filename>     -- Name of wind file")
   CALL WrScr("     options:     "//SwChar//"type[<type>]  -- type of the file, where <type> is               [N/A]")
   CALL WrScr("                                    HH    -- HubHeight                                   ")
   CALL WrScr("                                    FF    -- Full Field                                  ")
   CALL WrScr("                                    UD    -- User Defined                                ")
   CALL WrScr("                                    FD    -- 4-dimensional (.les)                        ")
   CALL WrScr("                                    CTP   -- Coherent turbulence wind field on top of another")
   CALL WrScr("                                    HAWC  -- HAWC formatted file                         ")
   CALL WrScr("                  "//SwChar//"height[#]     -- height of the hub                               [N/A]")
   CALL WrScr("                  "//SwChar//"width[#]      -- width of the windfield                          [N/A]")
   CALL WrScr("                  "//SwChar//"x[#:#]        -- range of x (#'s are reals)                      [N/A]")
   CALL WrScr("                  "//SwChar//"y[#:#]        -- range of y                                      [N/A]")
   CALL WrScr("                  "//SwChar//"z[#:#]        -- range in z (ground = 0.0)                       [N/A]")
   CALL WrScr("                  "//SwChar//"t[#:#]        -- range of time                                   [N/A]")
   CALL WrScr("                  "//SwChar//"xres[#]       -- resolution in x                                 [N/A]")
   CALL WrScr("                  "//SwChar//"yres[#]       -- resolution in y                                 [N/A]")
   CALL WrScr("                  "//SwChar//"zres[#]       -- resolution in z                                 [N/A]")
   CALL WrScr("                  "//SwChar//"tres[#]       -- resolution in time                              [N/A]")
   CALL WrScr("                  "//SwChar//"paraprint     -- make an output file for ParaView                [N/A]")
   CALL WrScr("                  "//SwChar//"summary       -- summarize in  .sum file                         [N/A]")
   CALL WrScr("                  "//SwChar//"fft[X,Y,Z]    -- an fft over all t at X,Y,Z (outputs .fft file)  [N/A]")
   CALL WrScr("                  "//SwChar//"points[FILE]  -- calculates at x,y,z coordinates specified in a  [N/A]")
   CALL WrScr("                                    white space delimited FILE")
   CALL WrScr("                  "//SwChar//"help          -- print this help menu")
   CALL WrScr("")
   CALL WrScr("   If the type is not specified, attempts are made to figure out what it is.")
   CALL WrScr("   Unspecified ranges and resolutions default to what is in the file.")
   CALL WrScr("   Features marked [N/A] have not been implimented in this version.")
!FIXME: Does the CTP get used with another? If so, specify in comment at end.
   CALL WrScr("")


END SUBROUTINE DispHelpText


!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
SUBROUTINE RetrieveArgs( Settings, SettingsFlags, ErrStat, ErrMsg )
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!
      ! Iterate through the input arguments !
      !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!

   USE NWTC_Library
   USE IfW_Driver_Types

   IMPLICIT NONE

      ! Storing the arguments
   TYPE( IfW_Driver_ArgFlags ),        INTENT(  OUT)  :: SettingsFlags        ! Flags indicating which arguments were specified
   TYPE( IfW_Driver_Args ),            INTENT(  OUT)  :: Settings             ! Arguments passed in

      ! Error Handling
   INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat
   CHARACTER(1024),                    INTENT(  OUT)  :: ErrMsg

      ! Local variables
   INTEGER(IntKi)                                     :: i
   CHARACTER(1024)                                    :: arg
   INTEGER(IntKi)                                     :: InputArgs
   LOGICAL                                            :: FileNameGiven


      ! initialize some things
   FileNameGiven = .FALSE.
   ErrStat = 0


      ! Check how many arguments are passed in
   InputArgs = COMMAND_ARGUMENT_COUNT()

      ! exit if we don't have enough
   IF (InputArgs == 0) THEN
      ErrMsg   = "Insufficient Arguments."
      ErrStat  = 3         ! FIXME:ErrStat should be according to template levels
      RETURN
   ENDIF


      ! Loop through all the arguments, and store them
   DO i=1,InputArgs
      CALL get_command_argument(i, arg)

         ! Check to see if it is a control parameter or the filename
      IF ( INDEX( SwChar, arg(1:1) ) > 0 ) THEN

            ! check to see if we asked for help
         IF ( arg(2:5) == "help" ) THEN
            CALL DispHelpText( ErrStat, ErrMsg )
            ErrStat  = 5   !FIXME:Errstat for abort
            ErrMsg   = ""
            !RETURN
            EXIT
         ENDIF

            ! Check the argument and put it where it belongs
            ! chop the SwChar off before passing the argument
         CALL ParseArg( Settings, SettingsFlags, arg(2:), ErrStat, ErrMsg )
         IF ( ErrStat == 1 ) CALL ProgWarn( ErrMsg )
         IF ( ErrStat > 5 ) RETURN        ! FIXME:ErrStat for abort


      ELSE

            ! since there is no switch character, assume it is the filename, unless we already set one
         IF ( FileNameGiven ) THEN
            ErrMsg   = "Multiple input filenames given: "//TRIM(Settings%InputFile)//", "//TRIM(arg)
            ErrStat  = 5      ! FIXME:Errstat for abort
            RETURN
         ELSE
            Settings%InputFile = TRIM(arg)
            FileNameGiven = .TRUE.
         ENDIF

      ENDIF
   END DO


      ! Check the arguments passed in:
   IF ( .NOT. FileNameGiven ) THEN
      ErrMsg   = "No filename given for file to open."
      ErrStat  = 5            ! FIXME:ErrStat for abort.
      RETURN
   ENDIF


   !-------------------------------------------------------------------------------
   !-------------------------------------------------------------------------------
      CONTAINS


   !-------------------------------------------------------------------------------
   FUNCTION StringToReal( StringIn, ErrStat )
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!
         ! Convert a string to a real number !
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!

      IMPLICIT NONE

         ! Error Handling
      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat

         ! Input
      CHARACTER(*),                       INTENT(IN   )  :: StringIn

         ! Returned value
      REAL(ReKi)                                         :: StringToReal

         ! Local Variables
      INTEGER(IntKi)                                     :: TempIO         ! Temporary variable to hold the error status

         read( StringIn, *, iostat=TempIO) StringToReal
         IF ( TempIO .ne. 0 ) ErrSTat  = 1   !FIXME:ErrStat

   END FUNCTION StringToReal



   !-------------------------------------------------------------------------------
   SUBROUTINE ParseArg( Settings, SettingsFlags, ThisArg, ErrStat, ErrMsg )
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!
         ! Parse and store the input argument  !
         !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-!

      USE NWTC_Library
      USE IfW_Driver_Types
      USE WindFile_Types

      IMPLICIT NONE

         ! Storing the arguments
      TYPE( IfW_Driver_ArgFlags ),        INTENT(INOUT)  :: SettingsFlags        ! Flags indicating which arguments were specified
      TYPE( IfW_Driver_Args ),            INTENT(INOUT)  :: Settings             ! Arguments passed in

      CHARACTER(*),                       INTENT(IN   )  :: ThisArg              ! The current argument

         ! Error Handling
      INTEGER(IntKi),                     INTENT(  OUT)  :: ErrStat
      CHARACTER(1024),                    INTENT(  OUT)  :: ErrMsg


         ! local variables
      INTEGER(IntKi)                                     :: Delim1               ! where the [ is
      INTEGER(IntKi)                                     :: Delim2               ! where the ] is
      INTEGER(IntKi)                                     :: DelimSep             ! where the : is
      INTEGER(IntKi)                                     :: DelimSep2            ! where the : is
      INTEGER(IntKi)                                     :: DelimSep3            ! where the : is
      REAL(ReKi)                                         :: TempReal             ! temp variable to hold a real
      INTEGER(IntKi)                                     :: TempIO               ! temp variable to store the IO error



         ! Initialize some things
      ErrStat  = 0
      TempIO   = 0

         ! Get the delimiters -- returns 0 if there isn't one
      Delim1   = INDEX(ThisArg,'[')
      Delim2   = INDEX(ThisArg,']')
      DelimSep = INDEX(ThisArg,':')


         ! check that if there is an opening bracket, then there is a closing one
      IF ( (Delim1 > 0 ) .and. (Delim2 < Delim1) ) THEN
         ErrMsg   = "Syntax error in option: '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
         ErrStat  = 1   !FIXME:ErrStat level ?
         RETURN
      ENDIF

         ! check that if there is a colon, then there are brackets
      IF ( (DelimSep > 0) .and. (Delim1 == 0) ) THEN
         ErrMsg   = "Syntax error in option: '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
         ErrStat  = 1   !FIXME:ErrStat level ?
         RETURN
      ENDIF


         ! Now go through the full list of possible options. Store as appropriate

         ! "type[#]"
      IF    ( ThisArg(1:Delim1) == "type["      ) THEN
         SELECT CASE (ThisArg(Delim1+1:Delim2-1))
            CASE ('HH')    ! hub height
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = HH_Wind

            CASE ('FF')    ! full field
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = FF_Wind

            CASE ('UD')    ! User Defined
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = UD_Wind

            CASE ('FD')    ! Four dimen
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = FD_Wind

            CASE ('CTP')   ! coherent turbulence
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = CTP_Wind

            CASE ('HAWC')  ! HAWC compatible
               SettingsFlags%WindFileType = .TRUE.
               Settings%WindFileType      = HAWC_Wind

            CASE DEFAULT
               ErrMsg   = "Invalid wind type. Ignoring option: '"//SwChar//TRIM(ThisArg)//"'."
               ErrStat  = 1   !FIXME:ErrStat

         END SELECT



         ! "height[#]"
      ELSEIF( ThisArg(1:Delim1) == "height["    ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Height = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Height = .TRUE.
            Settings%Height      = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Height = .FALSE.
         ENDIF


         ! "width[#]"
      ELSEIF( ThisArg(1:Delim1) == "width["     ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Width  = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Width  = .TRUE.
            Settings%Width       = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Width  = .FALSE.
         ENDIF



         ! "x[#:#]"
      ELSEIF( ThisArg(1:Delim1) == "x["         ) THEN

            ! First Value
         TempReal = StringToReal( ThisArg(Delim1+1:DelimSep-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Xrange = .TRUE.
            Settings%Xrange(1)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xrange = .FALSE.
         ENDIF

            ! Second Value
         TempReal = StringToReal( ThisArg(DelimSep+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Xrange = .TRUE.
            Settings%Xrange(2)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xrange = .FALSE.
         ENDIF

            ! Check the order of values
         IF ( Settings%Xrange(1) > Settings%Xrange(2) ) THEN
            ErrMsg   = "Unexpected order of values in option '"//SwChar//TRIM(ThisArg)//"'. Ingoring."
            ErrStat  = 1
            Settings%Xrange(1)   = 0.0
            Settings%Xrange(2)   = 0.0
            SettingsFlags%Xrange = .FALSE.
         ENDIF


         ! "y[#:#]"
      ELSEIF( ThisArg(1:Delim1) == "y["         ) THEN

            ! First Value
         TempReal = StringToReal( ThisArg(Delim1+1:DelimSep-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Yrange = .TRUE.
            Settings%Yrange(1)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yrange = .FALSE.
         ENDIF

            ! Second Value
         TempReal = StringToReal( ThisArg(DelimSep+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Yrange = .TRUE.
            Settings%Yrange(2)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yrange = .FALSE.
         ENDIF

            ! Check the order of values
         IF ( Settings%Yrange(1) > Settings%Yrange(2) ) THEN
            ErrMsg   = "Unexpected order of values in option '"//SwChar//TRIM(ThisArg)//"'. Ingoring."
            ErrStat  = 1
            Settings%Yrange(1)   = 0.0
            Settings%Yrange(2)   = 0.0
            SettingsFlags%Yrange = .FALSE.
         ENDIF


         ! "z[#:#]"
      ELSEIF( ThisArg(1:Delim1) == "z["         ) THEN

            ! First Value
         TempReal = StringToReal( ThisArg(Delim1+1:DelimSep-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Zrange = .TRUE.
            Settings%Zrange(1)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zrange = .FALSE.
         ENDIF

            ! Second Value
         TempReal = StringToReal( ThisArg(DelimSep+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zrange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Zrange = .TRUE.
            Settings%Zrange(2)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zrange = .FALSE.
         ENDIF

            ! Check the order of values
         IF ( Settings%Zrange(1) > Settings%Zrange(2) ) THEN
            ErrMsg   = "Unexpected order of values in option '"//SwChar//TRIM(ThisArg)//"'. Ingoring."
            ErrStat  = 1
            Settings%Zrange(1)   = 0.0
            Settings%Zrange(2)   = 0.0
            SettingsFlags%Zrange = .FALSE.
         ENDIF


         ! "t[#:#]"
      ELSEIF( ThisArg(1:Delim1) == "t["         ) THEN

            ! First Value
         TempReal = StringToReal( ThisArg(Delim1+1:DelimSep-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Trange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Trange = .TRUE.
            Settings%Trange(1)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Trange = .FALSE.
         ENDIF

            ! Second Value
         TempReal = StringToReal( ThisArg(DelimSep+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Trange = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Trange = .TRUE.
            Settings%Trange(2)   = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Trange = .FALSE.
         ENDIF

            ! Check the order of values
         IF ( Settings%Trange(1) > Settings%Trange(2) ) THEN
            ErrMsg   = "Unexpected order of values in option '"//SwChar//TRIM(ThisArg)//"'. Ingoring."
            Settings%Trange(1)   = 0.0
            Settings%Trange(2)   = 0.0
            ErrStat  = 1
            SettingsFlags%Trange = .FALSE.
         ENDIF


         ! "xres[#]"
      ELSEIF( ThisArg(1:Delim1) == "xres["      ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xres   = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Xres   = .TRUE.
            Settings%Xres        = abs(TempReal)
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Xres   = .FALSE.
         ENDIF

         ! "yres[#]"
      ELSEIF( ThisArg(1:Delim1) == "yres["      ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yres   = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Yres   = .TRUE.
            Settings%Yres        = abs(TempReal)
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Yres   = .FALSE.
         ENDIF

         ! "zres[#]"
      ELSEIF( ThisArg(1:Delim1) == "zres["      ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zres   = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Zres   = .TRUE.
            Settings%Zres        = abs(TempReal)
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Zres   = .FALSE.
         ENDIF

         ! "tres[#]"
      ELSEIF( ThisArg(1:Delim1) == "tres["      ) THEN
         TempReal = StringToReal( ThisArg(Delim1+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Tres   = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%Tres   = .TRUE.
            Settings%Tres        = abs(TempReal)
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%Tres   = .FALSE.
         ENDIF

         ! "paraprint"
      ELSEIF( ThisArg(1:9)      == "paraprint"  ) THEN
         SettingsFlags%ParaPrint = .TRUE.
         ErrMsg   = "Feature not implimented. Ignoring option '"//SwChar//TRIM(ThisArg)//"'."
         ErrStat  = 1      !FIXME:ErrStat

         ! "summary"
      ELSEIF( ThisArg(1:8)      == "summary"    ) THEN
         SettingsFlags%Summary   = .TRUE.
         ErrMsg   = "Feature not implimented. Ignoring option '"//SwChar//TRIM(ThisArg)//"'."
         ErrStat  = 1      !FIXME:ErrStat

         ! "fft[X,Y,Z]"
      ELSEIF( ThisArg(1:Delim1) == "fft["       ) THEN
         DelimSep = INDEX(ThisArg,',')
         DelimSep2= INDEX(ThisArg(DelimSep+1:),',') + DelimSep

            ! First Value
         TempReal = StringToReal( ThisArg(Delim1+1:DelimSep-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%fft    = .TRUE.
            Settings%fft(1)      = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
         ENDIF

            ! Second Value
         TempReal = StringToReal( ThisArg(DelimSep+1:DelimSep2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%fft    = .TRUE.
            Settings%fft(2)      = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
         ENDIF

            ! Third Value
         TempReal = StringToReal( ThisArg(DelimSep2+1:Delim2-1), ErrStat )
         IF ( ErrStat .eq. 1 ) THEN
            ErrMsg   = "Invalid number in option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
            RETURN
         ELSEIF ( ErrStat .eq. 0 ) THEN
            SettingsFlags%fft    = .TRUE.
            Settings%fft(3)      = TempReal
         ELSE
            ErrMsg   = "Something failed in parsing option '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
            SettingsFlags%fft    = .FALSE.
         ENDIF

         ! "points[FILE]"
      ELSEIF( ThisArg(1:Delim1) == "points["    ) THEN
         SettingsFlags%pointsfile= .TRUE.
         Settings%pointsfile     = ThisArg(Delim1+1:Delim2-1)

      ELSE
         ErrMsg  = "Unrecognized option: '"//SwChar//TRIM(ThisArg)//"'. Ignoring."
         ErrStat = 1    !FIXME:ErrStat should be for warning, but continue.
      ENDIF

   END SUBROUTINE ParseArg
   !-------------------------------------------------------------------------------



END SUBROUTINE RetrieveArgs

!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
END MODULE Ifw_Driver_Subs
