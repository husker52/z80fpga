echo off
REM --batch file to build VHDL Projects
REM --Dan Konz Sept. 24, 2007

rem get batch file path from command line args
SET simpath=%~dp0		
echo "**** batch path is ****"
echo %simpath%
echo "**** system path is ****"

rem run from simulation directory
cd %simpath%

vsim work.ser_pid_tb 
