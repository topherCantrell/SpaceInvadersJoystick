' TODO
'
' The player shots going up the screen wobble a bit. This is probably a bad algorithm in the
' hardware shift register (only used for shots).

CON

        _STACK    = 200
        _clkmode        = xtal1 + pll16x
        _xinfreq        = 5_000_000                   
                     
OBJ

  video  : "SIVideo"     ' Black and White raster
  ports  : "SIPorts"     ' I/O ports and interrupt manager
  snd    : "SISound"     ' Discrete sound simulator (uses SIDCog)
  cpu    : "CPU_8080"    ' 8080 CPU
    
  bin    : "invaders_bin"    ' The Space Invaders rom
  'bin    : "invdelux_bin"    ' The Space Invaders Delux rom

VAR   
        
  ' Triggered I/O
  long  ioCommand 
  long  ioAddress  
  long  ioValue
    
  ' CPU Control    
  long  command  
  long  interrupt
  long  cycleCount
  long  dumpBuffer[16]
 
  ' Current sound value
  long soundValue

  ' Current video row number
  long rowNum
      
  ' ROM/RAM and screen memory
  'byte rom[8*1024]            '0000-1FFF
  byte ram[1*1024]            '2000-23FF
  byte screenMemory[7*1024]   '2400-3FFF
  byte rom2[2*1024]           ' Space Invaders 2 has an extra ROM chip  
  
PUB start | i,p

  ioCommand  :=0   ' Clear I/O command   
  soundValue :=0   ' No sound
  rowNum     :=0   ' Video drawing row 0      
  ports.start(@ioCommand, @soundValue, @rowNum, @command,0)

  ' SI display is 224 rows
  ' NTCS display gives us 208 rows
  ' 224 - 208 = 16 less rows than original SI
  ' We'll divide those into 8 on each side
  ' The SI game stays within this range
  video.start(24,@screenMemory+256,@rowNum)
    
  ' ROM is a separate area so we can compile in the program for testing
  ' Everything else is filtered into VAR RAM beginning with ram[]  
  command    :=0   ' Start running
  interrupt  :=0   ' No interrupt pending
  cpu.start( 0,0, 0,0,    0,bin.getData,  $2000,@ram-$2000,  @ioCommand,@command)

  ' Start the SID COG and then this COG becomes the SI sound processor
  snd.Main(@soundValue,4,5)    

  ' Never getting here (code remains for debugging)
  repeat

{
PUB PauseMSec(Duration)  

''  Pause execution for specified milliseconds.
''  This routine is based on the set clock frequency.
''  
''  params:  Duration = number of milliseconds to delay                                                                                               
''  return:  none
  
  waitcnt(((clkfreq / 1_000 * Duration - 3932) #> 381) + cnt)

  return
}              