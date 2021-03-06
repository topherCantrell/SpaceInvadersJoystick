DOC

'Input:
'Port 0
'   bit 0 = DIP4       : 0
'   bit 1 = Always 1   : 1  
'   bit 2 = Always 1   : 1
'   bit 3 = Always 1   : 1
'   bit 4 = Fire       : Fire button
'   bit 5 = Left       : Left
'   bit 6 = Right      : Right
'   bit 7 =            : 0
'Port 1
'   bit 0 = CREDIT     : Reset
'   bit 1 = 2P start   : Select
'   bit 2 = 1P start   : Start
'   bit 3 = Always 1   : 1
'   bit 4 = 1P shot    : Fire button
'   bit 5 = 1P left    : Left
'   bit 6 = 1P right   : Right
'   bit 7 =            : 0
'Port 2
'   bit 0 = DIP3 00 = 3 ships  10 = 5 ships  : 0
'   bit 1 = DIP5 01 = 4 ships  11 = 6 ships  : 0
'   bit 2 = Tilt        : 1 if both starts are pressed
'   bit 3 = DIP6 0 = extra ship at 1500, 1 = extra ship at 1000 : 0
'   bit 4 = P2 shot     : Fire button
'   bit 5 = P2 left     : Left
'   bit 6 = P2 right    : Right
'   bit 7 = DIP7 Coin info displayed in demo screen 0=ON  : 0
' Port 3
'   bit 0-7 Shift register data
'
' Output
'Port 2:
'  bit 0,1,2 Shift amount
'Port 3:
'  bit 0=UFO (repeats)        SX0 0.raw
'  bit 1=Shot                 SX1 1.raw
'  bit 2=Flash (player die)   SX2 2.raw
'  bit 3=Invader die          SX3 3.raw
'  bit 4=Extended play        SX4
'  bit 5= AMP enable          SX5
'  bit 6= NC (not wired)
'  bit 7= NC (not wired)
'Port 4:
'  bit 0-7 shift data (LSB on 1st write, MSB on 2nd)
'Port 5:
'  bit 0=Fleet movement 1     SX6 4.raw
'  bit 1=Fleet movement 2     SX7 5.raw
'  bit 2=Fleet movement 3     SX8 6.raw
'  bit 3=Fleet movement 4     SX9 7.raw
'  bit 4=UFO Hit              SX10 8.raw
'  bit 5= NC (Cocktail mode control ... to flip screen)
'  bit 6= NC (not wired)
'  bit 7= NC (not wired)
'
'Port 6:
'  Watchdog ... read or write to reset


''##### HARDWARE #####
''
'' The Joystick-switches, buttons and LED are connected to the
'' Propeller as follows:
''                           
''                        3.3V
''           ┌───┳───┳───╋───┳────┳───┳───┐
''           R3 R4 R5 R6 R28 R7 R8 R9 ─ All 10KΩ
'' P21 ─────┻───┼───┼───┼───┼────┼───┼───┼────┐ Up
'' P16 ─────────┻───┼───┼───┼────┼───┼───┼────┫ Down
'' P14 ─────────────┻───┼───┼────┼───┼───┼────┫ Left
'' P17 ─────────────────┻───┼────┼───┼───┼────┫ Right    Joystick switches
'' P15 ─────────────────────┻────┼───┼───┼────┫ Fire     and buttons
'' P18 ──────────────────────────┻───┼───┼────┫ Start
'' P19 ──────────────────────────────┻───┼────┫ Select
'' P20 ──────────────────────────────────┻────┫ Reset
''                                               
'' LED is on P27
  
PUB start(ioPtr, soundRegPtr, rowNumberPtr, commandPtr, keyParams)

  keyParamsP := keyParams ' Point to key-states
  
  portOpP    := ioPtr
  portAddrP  := ioPtr+4
  portValueP := ioPtr+8

  soundRegP  := soundRegPtr
  rowNumberP := rowNumberPtr
  interruptP := commandPtr+4
  cycleCntP  := commandPtr+8

  return cognew(@entry,0)
   
DAT

         org

entry

mainClear
         mov       tmp,#0              ' Ack any ...
         wrlong    tmp,portOpP         ' ... I/O request

main       
         rdlong    cmd,rowNumberP          ' Get the row-number from the video system
         ' If the current is >=104 and last <104 then upper screen interrupt
         cmp       cmd,#104 wc, wz         ' Current row less than mid-screen?
 if_b    jmp       #intLogicA              ' Yes ... can't be mid screen
         cmp       lastRowNum,#104 wc, wz  ' Last time through were we less?
 if_ae   jmp       #intLogicA              ' No ... can't be mid screen               
         wrlong    C_1008,interruptP       ' IRQ ... mid-screen interrupt 
         jmp       #intLogicB              ' Done with interrupts                    
intLogicA
         cmp       cmd,lastRowNum wc, wz   ' Has current row wrapped below last?
  if_ae  jmp       #intLogicB              ' No ... haven't crossed the end of the screen  
         wrlong    C_2010,interruptP       ' NMI ... end-screen interrupt                
intLogicB
         mov       lastRowNum,cmd          ' Remember this row next time through

' Interrupts based on CPU cycle count (easier to debug)
{
         rdlong    cmd,cycleCntP           ' Get the current CPU cycle count
         shr       cmd,#10                 ' Divide by 4096
         cmp       cmd,lastCycleCnt wz     ' Have 4096 cycles passed since last time?
         mov       lastCycleCnt,cmd        ' New last cycle count (does not affect Z)
  if_z   jmp       #intLogicB              ' Not time for an interrupt ... skip on
         and       cmd,#1 wz               ' Odd or even?
  if_z   jmp       #intLogicA              ' Even ... do NMI interrupt
         wrlong    C_1008,interruptP       ' Odd ... do IRQ interrupt
         jmp       #intLogicB              ' Out
intLogicA                                  '
         wrlong    C_2010,interruptP       ' Do NMI interrupt    
intLogicB                                  '
}
         
         rdlong    cmd,portOpP wz          ' Wait for a ...         
  if_z   jmp       #main                   ' ... request          

         cmp       cmd,#4 wz               ' 4 for I/O out (write)
         rdlong    cmd,portAddrP           ' Get port address (doesn't change Z) 
  if_z   jmp       #doWrites               ' Do write  or fall into do read           
                              
doReads  cmp       cmd,#3 wc, wz           ' Range ...
  if_a   jmp       #mainClear              ' ... check
         add       cmd,#readPortsTable     ' Jump to ...
         jmp       cmd                     ' ... target "read port"

doWrites cmp       cmd,#6 wc,wz             ' Range ...
  if_a   jmp       #mainClear               ' ... check
         add       cmd,#writePortsTable     ' Jump to ...
         jmp       cmd                      ' ... target "write port"  

readPortsTable
         jmp       #readPort0
         jmp       #readPort1
         jmp       #readPort2
         jmp       #readPort3
writePortsTable
         jmp       #mainClear
         jmp       #mainClear
         jmp       #writePort2
         jmp       #writePort3
         jmp       #writePort4
         jmp       #writePort5
         jmp       #writePort6


' The ports all map the inputs to the same bits. Some ports have
' differnt 1s and 0s
'
'   bit 0 = CREDIT     :C Reset            
'   bit 1 = 2P start   :2 Select           
'   bit 2 = 1P start   :1 Start            
'   bit 3 = Always 1   :  1
'   bit 4 = 1P shot    :F Fire button      
'   bit 5 = 1P left    :L Left             
'   bit 6 = 1P right   :R Right            
'   bit 7 =            :  0
'
' xxxxxxxx_xxLC21DR_FUxxxxxx_xxxxxxxx     ' Read from ina
'                            LC21DRFU     ' Shifted ina
'                            .RLF.12C     ' Final mapping
     
readKeys
         mov      tmp,#0                   ' Clear result

         mov      tmp2,keyParamsP
         add      tmp2,#4
         rdlong   tmp3,tmp2

         and      tmp3,#1 nr, wz
  if_nz  or       tmp,#%0001_0000
         shr      tmp3,#17
         and      tmp3,#1 nr, wz
  if_nz  or       tmp,#%0000_0100
         and      tmp3,#2 nr, wz
  if_nz  or       tmp,#%0000_0010
         and      tmp3,#$10 nr, wz
  if_nz  or       tmp,#%0000_0001

         mov      tmp2,keyParamsP
         add      tmp2,#24
         rdlong   tmp3,tmp2

         and      tmp3,#1 nr, wz
  if_nz  or       tmp,#%0010_0000
         and      tmp3,#2 nr, wz
  if_nz  or       tmp,#%0100_0000         

readKeys_ret
         ret
         
readStick
         mov      tmp,#0                   ' Clear result
         mov      cmd,ina                  ' Get the bits from the stick
         shr      cmd,#14                  ' Into position
         xor      cmd,#%11111111
                          
         ' The Great Bit Shuffle
         
         and      cmd,#2 wz,nr
   if_nz or       tmp,#16                       
         and      cmd,#4 wz,nr
   if_nz or       tmp,#64                      
         and      cmd,#16 wz,nr
   if_nz or       tmp,#4                       
         and      cmd,#32 wz,nr
   if_nz or       tmp,#2                       
         and      cmd,#64 wz,nr
   if_nz or       tmp,#1                         
         and      cmd,#128 wz,nr
   if_nz or       tmp,#32

readStick_ret
         ret                          


' Switches ports
   
readPort0

         cmp       keyParamsP,#0 wz    ' Keyboard or Stick?
  if_z   jmp       #:doStick           ' Go read the stick
         call      #readKeys           ' Read the keyboard
         jmp       #:readDone          ' Process
:doStick                               '
         call      #readStick          ' Read the stick
:readDone

         andn      tmp,#$81

' Space Invaders 1 hardware returs all three of these bits as 1, but the software ignores them
' Space Invaders 2 will reset if xxxx1x1x is ....1.1.
' We leave the upper bit off so SI and SI2 will both work with this code
         
         'or        tmp,#$0E
         or        tmp,#$06            ' Space Invaders 2 will kill itself if ....1x1x         
         wrlong    tmp,portValueP
         jmp       #mainClear             

readPort1
         cmp       keyParamsP,#0 wz    ' Keyboard or Stick?
  if_z   jmp       #:doStick           ' Go read the stick
         call      #readKeys           ' Read the keyboard
         jmp       #:readDone          ' Process
:doStick                               '
         call      #readStick          ' Read the stick
:readDone

         andn     tmp,#$80
         or       tmp,#$08
         wrlong   tmp,portValueP
         jmp      #mainClear
         
readPort2
         cmp       keyParamsP,#0 wz    ' Keyboard or Stick?
  if_z   jmp       #:doStick           ' Go read the stick
         call      #readKeys           ' Read the keyboard
         jmp       #:readDone          ' Process
:doStick                               '
         call      #readStick          ' Read the stick
:readDone

         mov      cmd,tmp              ' Get status of ...
         and      cmd,#$06             ' ... both starts                           
         andn     tmp,#$8F             ' Turn off all but 3 bits in return
         cmp      cmd,#$06 wz          ' If both starts are pressed ...
  if_e   or       tmp,#4               ' ... then set the tilt
  
         wrlong   tmp,portValueP        ' Write the bit pattern
         jmp      #mainClear           ' Done
                                                                           

' Watchdog port

writePort6
         ' Watchdog ... just ignore
         jmp      #mainClear


         
' Sound register ports

writePort3
         rdlong   tmp,portValueP       ' Read the new LSB
         and      tmp,#$FF             ' Mask it to 8 bits
         andn     soundValue,#$FF      ' Drop LSB of sound value
         or       soundValue,tmp       ' OR in new LSB
         wrlong   soundValue,soundRegP ' Write the new sound bits 
         jmp      #mainClear           ' Done
         
writePort5
         rdlong   tmp,portValueP       ' Read the new MSB
         and      tmp,#$FF             ' Mask it to 8 bits
         shl      tmp,#8               ' Move it to MSB position
         and      soundValue,#$FF      ' Drop MSB of sound value
         or       soundValue,tmp       ' OR in new MSB
         wrlong   soundValue,soundRegP ' Write the new sound bits
         jmp      #mainClear           ' Done



' Shift register ports                                                

writePort4
         ' Move MSB to LSB
         ' Read new MSB
         shr      shiftValue,#8            ' Move the MSB to LSB
         rdlong   tmp,portValueP           ' Read the new MSB
         shl      tmp,#8                   ' Shift it into final position
         or       shiftValue,tmp           ' Set the value's MSB
         jmp      #mainClear               ' Done
               
writePort2
         rdlong   shiftAmount,portValueP   ' Get the shift amount
         and      shiftAmount,#7           ' Just need the lower 3 bits
         jmp      #mainClear               ' Done

readPort3
         mov      tmp,shiftValue           ' The value of the shift register
         shl      tmp,shiftAmount          ' Go left first to chop off upper bits
         shr      tmp,#8                   ' Now go right to get a 1-byte value
         and      tmp,#$FF                 ' Mask off any hanging bits to the left
         wrlong   tmp,portValueP           ' Write back the result         
         jmp      #mainClear               ' Done                    
         
          
cmd          long $0
tmp          long $0
tmp2         long $0
tmp3         long $0

lastCycleCnt long $0
lastRowNum   long $0

soundValue   long $0   

shiftValue   long $0
shiftAmount  long $0

C_1008       long $1008 ' CPU_8080 value for IRQ
C_2010       long $2010 ' CPU_8080 value for NMI

portOpP      long $0_0
portAddrP    long $0_0
portValueP   long $0_0

soundRegP    long $0_0

rowNumberP   long $0_0

interruptP   long $0_0
cycleCntP    long $0_0

keyParamsP   long $0_0