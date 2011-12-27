echo off
rem ----------------------------------------------------------------------------
rem --   Copyright (C) 2011 LI-COR Biosciences, Lincoln, Nebraska, USA.
rem --   All rights reserved.  An unpublished and CONFIDENTIAL work.  
rem --   Reproduction, adaptation or translation without prior written 
rem --   permission of LI-COR Biosciences is prohibited.
rem ----------------------------------------------------------------------------
rem -- filename                 : build_rtl.bat
rem -- Author                   : Brad Riensche
rem -- Date                     : 30-nov-2011

rem get batch file path from command line args
set oldpath=%CD%
SET simpath=%~dp0		

echo "batch path is %simpath%"

REM delete library so that no old complided entities exist.  
vdel -all -lib work

REM re-create libary and complile to it. 
vlib work
vmap work work

REM compile source files
echo on
rem vcom ..\..\altera\de0_top.vhd
vcom ..\..\opencores\binarytobcd\bcdconv.vhd
vcom ..\..\opencores\binarytobcd\digit.vhd


rem compile test benches
vcom ..\..\opencores\binarytobcd\bcdconvtb.vhd
echo off

REM Start Vendor dependant entities
rem vcom ..\..\src\global_buffer_igloo.vhd -93 
REM End vendor dependant entities
cd %oldpath%

