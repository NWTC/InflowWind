ECHO OFF
@SET Editor=NotePad.EXE

SET PROG_EXE=..\bin\InflowWind_Driver_win32.exe
rem SET PROG_EXE=..\bin\InflowWind_Driver_gwin32.exe
SET COMPARE=FC
SET OLD_RESULTS=.\Results
@SET  POUNDS=#############################################################################################

@IF EXIST CertTest.out  DEL CertTest.out

echo %pounds% >> CertTest.out
echo %pounds%

SET TEST=Test001--steady
%PROG_EXE% %TEST%.dvr
%COMPARE% %TEST%.IfW.sum                   %OLD_RESULTS%\%TEST%.IfW.sum               >> CertTest.out
%COMPARE% %TEST%.WindGrid.out              %OLD_RESULTS%\%TEST%.WindGrid.out          >> CertTest.out
%COMPARE% Test001--points.Velocity.dat     %OLD_RESULTS%\Test001--points.Velocity.dat >> CertTest.out
echo %pounds% >> CertTest.out
echo %pounds%

SET TEST=Test002--Uniform
%PROG_EXE% %TEST%.dvr
%COMPARE% %TEST%.IfW.sum                   %OLD_RESULTS%\%TEST%.IfW.sum               >> CertTest.out
%COMPARE% %TEST%.WindGrid.out              %OLD_RESULTS%\%TEST%.WindGrid.out          >> CertTest.out

echo %pounds% >> CertTest.out
echo %pounds%

SET TEST=Test003--TSFF
%PROG_EXE% %TEST%.dvr
%COMPARE% %TEST%.IfW.sum                   %OLD_RESULTS%\%TEST%.IfW.sum               >> CertTest.out
%COMPARE% %TEST%.WindGrid.out              %OLD_RESULTS%\%TEST%.WindGrid.out          >> CertTest.out
echo %pounds% >> CertTest.out
echo %pounds%

SET TEST=Test004--BladedFF
%PROG_EXE% %TEST%.dvr
%COMPARE% %TEST%.IfW.sum                   %OLD_RESULTS%\%TEST%.IfW.sum               >> CertTest.out
%COMPARE% %TEST%.WindGrid.out              %OLD_RESULTS%\%TEST%.WindGrid.out          >> CertTest.out
%COMPARE% Test004--points.Velocity.dat     %OLD_RESULTS%\Test004--points.Velocity.dat >> CertTest.out
echo %pounds% >> CertTest.out
echo %pounds%

%Editor% CertTest.out