CON

        _STACK    = 200
        _clkmode        = xtal1 + pll16x
        _xinfreq        = 5_000_000
         playRate = 21 'Hz             
                     
OBJ

  video  : "SIVideo"
  ports  : "SIPorts"
  snd    : "SISound"  
  cpu    : "CPU_8080"
  disk   : "Disk_hdw"

VAR

  ' Disk controller
  long diskCommand
  long diskSectorAddress
  long diskMemoryAddress

  ' Shared between CPU and PORTS
  long portCommand
  long portAddress
  long portValue

  ' Shared between PORTS and SND
  long soundValue

  ' Shared between PORTS and VIDEO
  long rowNum 

  ' Shared between PORTS and CPU
  long interrupt

  ' Used by CPU and VIDEO (screen memory)
  byte rom[8*1024]            '0000-1FFF
  byte ram[1*1024]            '2000-23FF
  byte screenMemory[7*1024]   '2400-3FFF
  byte rom2[2*1024]           ' Space Invaders 2 has an extra ROM chip

  ' CPU registers
  long regs[4] 
  long cpuControl
  long cpuOpReg           
  
  ' Parameters for the video COG
  long v_scrPtr
  long v_rowNum

  ' Parameters for the CPU
  long p_memory_base     
  long p_io_base    
  long p_interrupt              

  ' Parameters for the PORTS
  long o_portOp
  long o_rowNum
  long o_soundValue
  long o_interrupt

  'long debugPtr             
  
PUB start | i,p
   

 ' debugPtr := SCREEN
  
  ' Start disk controller and mount the card
  diskCommand := 1
  diskMemoryAddress := @rom[0] ' Memory area needed by the mount
  disk.start(5,@diskCommand)  
  repeat while diskCommand<>0

  ' Read 16 chunks (512*36 = 18K)
  diskSectorAddress := 0  
  diskCommand := $1_24
  diskMemoryAddress := @rom[0]
  repeat while diskCommand<>0   

  portCommand := 0
  rowNum := 0
  soundValue :=0
  interrupt := 0

  o_portOp := @portCommand
  o_rowNum := @rowNum
  o_soundValue := @soundValue
  o_interrupt := @interrupt
  ports.start(6,@o_portOp)

  ' SI display is 224 rows
  ' NTCS display gives us 208 rows
  ' 224 - 208 = 16 less rows that original SI
  ' We'll divide those into 8 on each side
  ' The SI game stays within this range
  v_scrPtr := @screenMemory + 256
  v_rowNum := @rowNum       
  video.start(7,@v_scrPtr)
  
  p_memory_base := @rom 
  p_io_base := @portCommand  
  p_interrupt := @interrupt   
  cpu.start(@p_memory_base)

  ' Start the SID COG and then this COG becomes the SI sound processor
  snd.Main(@soundValue)


{     
 
PUB writePort(address,value)

  portAddress := address
  portValue := value
  portCommand := 1
  repeat while (portCommand<>0)

PUB readPort(address)
  portAddress := address
  portCommand :=2
  repeat while (portCommand<>0)
  return portValue

PUB debugDigit(value) | p, i
    
  value := value & %1111  
  if value>9
    p := SI_CHARS
    value := value - 10
  else
    p := SI_DIGS
    
  value := value *5
  p := p + value

  repeat i from 1 to 5    
    ram[debugPtr] := rom[p]  
    debugPtr := debugPtr + 32
    p := p + 1

  debugPtr := debugPtr + 32*3  

PUB debugByte(value) | a
  a := value >> 4
  debugDigit(a)
  debugDigit(value)  

PUB debugWord(value) | a
  a := value >>8
  debugByte(a)
  debugByte(value)
}