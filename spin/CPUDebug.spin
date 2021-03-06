OBJ
    
  PST    : "Parallax Serial Terminal"  

VAR   
        
  ' CPU Control    
  long  commandP
  long  cycleCountP
  long  dumpBufferP
    
PUB start(cpuParams) | i,p

  commandP    := cpuParams
  cycleCountP := cpuParam+8
  dumpBufferP := cpuParams+12

  PauseMSec(2_000)  'A small delay to allow time to switch to the terminal application after loading the device  
  PST.Start(115_200) 
  PST.Home
  PST.Clear
  PST.Str(string("Started.",13))  

  repeat
  
    repeat while command==0
    PST.hex(command,8)
    PST.char(" ")
    Dump
    c := PST.charIn
    PST.char(13)
    if c=="r"
      command := $10
    else
      command := $12

PUB Dump
  PST.str(string("AF: "))
  PST.hex(dumpBuffer[0],2)
  PST.char(" ")
  PST.hex(dumpBuffer[1],2)
  PST.char(13)

  PST.str(string("BC: "))
  PST.hex(dumpBuffer[2],2)
  PST.char(" ")
  PST.hex(dumpBuffer[3],2)
  PST.char(13)

  PST.str(string("DE: "))
  PST.hex(dumpBuffer[4],2)
  PST.char(" ")
  PST.hex(dumpBuffer[5],2)
  PST.char(13)

  PST.str(string("HL: "))
  PST.hex(dumpBuffer[6],2)
  PST.char(" ")
  PST.hex(dumpBuffer[7],2)
  PST.char(13)

  PST.str(string("SP: "))
  PST.hex(dumpBuffer[8],4)
  PST.char(13)

  PST.str(string("PC: "))
  PST.hex(dumpBuffer[9],4)
  PST.char(13)

PUB PauseMSec(Duration)  

''  Pause execution for specified milliseconds.
''  This routine is based on the set clock frequency.
''  
''  params:  Duration = number of milliseconds to delay                                                                                               
''  return:  none
  
  waitcnt(((clkfreq / 1_000 * Duration - 3932) #> 381) + cnt)

  return
}              