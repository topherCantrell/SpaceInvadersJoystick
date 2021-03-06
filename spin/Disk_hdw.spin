''##### MEMORY MAPPED INTERFACE #####

' Clocks per second is 4C4B400 (80000000)
' Reading 4*512 takes 4DF10 clocks   (319248)   .004 sec to move 2K  (500K/sec) 
' Writing 4*512 takes ECF10 clocks   (970512)   .012 sec to move 2K  (167K/sec)
                                         
''
'' long command (trigger goes to 0 when finished)
'' long sectorAddress
'' long memoryAddress
''
'' command = 00_00000001 Mount SD card (find first data sector)
''         = 01_nnnnnnnn Read n 512-byte sectors from address to memory
''         = 10_nnnnnnnn Write n 512-byte from memory to address
'
'' return status is written to "sectorAddress" when finished
''
''##### HARDWARE #####
''
''The SD card is connected to the Propeller as follows:
''
''               3.3V
''                                                    3.3V
''     220Ω R11    R10 10KΩ                             
'' Pn   ───────┻────── DO   (SD card SPI signal)     └── Vdd  (SD power)              
'' Pn+1 ──────────────── SCLK (SD card SPI signal)     ┌── Vss  (SD power)
'' Pn+2 ──────────────── DI   (SD card SPI signal)     ┣── Vss2 (SD power)
'' Pn+3 ──────────────── CS   (SD card SPI signal)     
''
'' These pins can be changed by modifying variables at the end of the assembly code.
''
''##### SPI AND FAT #####
''
'' The SD SPI communications code was taken from Tomas Rokicki's work
'' available on the Parallax Propeller Object Exchange in the object:
'' "FAT16 routines with secure digital card layer"
''
'' The DiskCOG parses the FAT information in a limited way. It reads enough
'' to find the first file's starting point. It then assumes that sectors
'' are sequential (not fragmented) on the disk from that point on. This
'' requires the user to reformat the SD card before loading new code. This
'' will be corrected in future releases by caching the location of the
'' file's sectors in a table in reserved RAM. 
''
pub start(cog, paramBlock)
'' Start the DiskCOG

   coginit(cog,@DiskCOG,paramBlock)
   
DAT      
         org 0

DiskCOG             
                                       '  Parameter block (aligned on LONG boundary): 
         mov       command,par         '  4 command 
         mov       sectorAddress,par   '    00_00000001   Mount SD card (find first data sector)    
         add       sectorAddress,#4    '    01_nnnnnnnn   Read n 512-byte sectors from address to memory
         mov       memoryPtr,par       '    10_nnnnnnnn   Write n 512-byte from memory to address
         add       memoryPtr,#8        '  4 sectorAddress
                                       '  4 memortyPtr   

main     rdlong    com,command wz      ' Wait on a ...
  if_z   jmp       #main               ' ... non-zero command

         mov       count,com           ' Get sector ...
         and       count,#%11111111   ' ... count
         shr       com,#8 wz           ' Get command
         
  if_z   jmp      #SD_Mount            ' 0 = MOUNT (memoryPtr is a scratch 512 byte buffer)
         cmp      com,#1 wz            ' 1 = ...             
  if_z   jmp      #SD_MultiRead        ' ... READ
         cmp      com,#2 wz            ' 2 = ...
  if_z   jmp      #SD_MultiWrite       ' ... WRITE

finalERR mov      tmp,#0               ' 0 is FAIL
         jmp      #finalN
finalOK  mov      tmp,#1               ' 1 is OK
finalN   wrlong   tmp, sectorAddress   ' Error status
         mov      com,#0               ' Clear command ...
         wrlong   com,command          ' ... (we are done)
         jmp      #main                ' Next command

SD_MultiWrite
        rdlong   parptr,memoryPtr      ' Get the memory pointer
        rdlong   parptr2,sectorAddress ' Get the sector number
        add      parptr2,firstDataSector
multiWrite
        call     #SD_Write             ' Write one sector
        add      parptr2,#1            ' Next sector
        add      parptr,sector         ' Next 512 bytes
        djnz     count,#multiWrite     ' Loop over all
        jmp      #finalOK              ' OK

SD_MultiRead
        rdlong   parptr,memoryPtr       ' Get the memory pointer
        rdlong   parptr2,sectorAddress  ' Get the sector number
        add      parptr2,firstDataSector
multiRead
        call     #SD_Read               ' Read one sector
        add      parptr2,#1             ' Next sector
        add      parptr,sector          ' Next 512 bytes
        djnz     count,#multiRead       ' Loop over all
        jmp      #finalOK               ' OK
 
' ------------------------------------------------------------------------------------------------------
' ------------------------------------------------------------------------------------------------------
' ------------------------------------------------------------------------------------------------------

' Modified from sdspiqasm  "FAT16 routines with secure digital card layer"

SD_Mount

        mov acca,#1
        shl acca,di        
        or dira,acca
        mov acca,#1
        shl acca,clk
        or dira,acca
        mov acca,#1
        shl acca,do
        mov domask,acca
        mov acca,#1
        shl acca,cs
        or dira,acca
        mov csmask,acca
        neg phsb,#1
        mov frqb,#0
        mov acca,nco
        add acca,clk
        mov ctra,acca
        mov acca,nco
        add acca,di
        mov ctrb,acca  
            
        mov ctr2,onek
oneloop
        call #sendiohi 
        djnz ctr2,#oneloop   
        mov starttime,cnt
        mov cmdo,#0
        mov cmdp,#0        
        call #cmd
        or outa,csmask
        call #sendiohi   

initloop
        mov cmdo,#55
        call #cmd
        mov cmdo,#41
        call #cmd
        or outa,csmask
        cmp accb,#1 wz
   if_z jmp #initloop                         
         
' reset frqa and the clock
'finished
        mov frqa,#0
        or outa,csmask
        neg phsb,#1
        call #sendiohi
'pause
        mov acca,#511
        add acca,cnt
        waitcnt acca,#0          

' Find the first FAT data sector on the disk ...
' We have to go through the MasterBootRecord, and
' account for FAT and FAT32

' For FAT32, the 2-byte fatSize is 0 and:
'   fatSize is the 4-byte value
'   rootEnt is sectorsPerCluster

' In the future, all this needs to be done in Boot.spin filling out a table
' of sector numbers. That makes the solution more flexible and takes all
' this math out of DiskCOG's code space

' 1st data sector = bootSectorNum + reserved + rootEnt + fatSize*numFats

         rdlong   ptr_hold,memoryPtr

         mov       parptr,ptr_hold    ' Scratch area
         mov       parptr2,#0          ' Read ...
         call      #SD_Read            ' ... Master Boot Record

         mov       parptr,ptr_hold      ' Look ...
         add       parptr,#510         ' ...
         rdword    tmp,parptr          ' ... for ...
         cmp       tmp,C_AA55 wz       ' ...
  if_nz  jmp       #finalERR           ' AA55 end

         mov       parptr,ptr_hold      ' Get ...
         add       parptr,#$1C6        ' ... boot ...
         rdbyte    parptr2,parptr      ' ... sector ...
         mov       firstDataSector,parptr2      ' ...  
         mov       parptr,ptr_hold      ' ... of first ...
         call      #SD_Read            ' ... partition

         mov       parptr,ptr_hold      ' Look ...
         add       parptr,#510         ' ... 
         rdword    tmp,parptr          ' ... for ...
         cmp       tmp,C_AA55 wz       ' ...
  if_nz  jmp       #finalERR            ' AA55 end

         mov       parptr,ptr_hold
         add       parptr,#13
         rdbyte    tmp6,parptr         ' tmp6 = sectorsPerCluster  

         mov       tmp,#14
         call      #readTwoByteValue
         add       firstDataSector,acca ' firstDataSector = boot sector + numberOfReserved  

         mov       tmp,#22
         call      #readTwoByteValue
         mov       tmp4,acca wz        ' tmp4 = sectors per fat                 
  if_nz  jmp       #mount1         

         mov       tmp,#36
         call      #readTwoByteValue
         mov       tmp4,acca
         mov       tmp,#38
         call      #readTwoByteValue
         shl       acca,#16
         add       tmp4,acca           ' tmp4 = sectors per fat (large for FAT32)
         mov       tmp5,tmp6           ' tmp5 = 1 cluster (root directory)
         jmp       #mount2             

mount1   mov       tmp,#17
         call      #readTwoByteValue
         mov       tmp5,acca
         shr       tmp5,#4        

mount2   add       firstDataSector,tmp5 ' account for root directory

         ' firstDataSector = firstDataSector + numFats*fatsize
         mov       tmp,ptr_hold
         add       tmp,#16
         rdword    t1,tmp
         mov       t2,tmp4
         call      #multiply
         add       firstDataSector,t1

         wrlong    firstDataSector, ptr_hold ' in case anyone cares         
                  
         jmp       #finalOK 

' --------------------------------
' tmp  = offset in sector
' acca = two-byte-value from offset
readTwoByteValue
         add       tmp,ptr_hold
         rdbyte    acca,tmp
         add       tmp,#1
         rdbyte    accb,tmp
         shl       accb,#8
         add       acca,accb
readTwoByteValue_ret
         ret

multiply mov       t3,#16
         shl       t2,#16
         shr       t1,#1 wc
mloop
  if_c   add       t1,t2 wc
         rcr       t1,#1 wc
         djnz      t3,#mloop         
multiply_ret
         ret       

SD_Write                  
' parptr2   = block address
' parptr    = RAM pointer
        mov starttime,cnt
        mov cmdo,#24
        mov cmdp,parptr2
        call #cmd
        mov phsb,#$fe
        call #sendio
        mov accb,parptr
        neg frqa,#1
        mov ctr2,sector
wbyte
        rdbyte phsb,accb
        shl phsb,#23
        add accb,#1
        mov ctr,#8
wbit    mov phsa,#8
        shl phsb,#1
        djnz ctr,#wbit
        djnz ctr2,#wbyte        
        neg phsb,#1
        call #sendiohi
        call #sendiohi
        call #readresp
        and accb,#$1f
        sub accb,#5
        mov rw_status,accb
        call #busy 
        mov frqa,#0 
        or outa,csmask
        neg phsb,#1
        call #sendiohi
'pause
        mov acca,#511
        add acca,cnt
        waitcnt acca,#0
SD_Write_ret
        ret     
        
SD_Read
' parptr2 = block address
' parptr  = RAM pointer
        mov starttime,cnt
        mov cmdo,#17
        mov cmdp,parptr2
        call #cmd
        call #readresp
        mov accb,parptr
        sub accb,#1
        mov ctr2,sector
rbyte
        mov phsa,hifreq
        mov frqa,freq
        add accb,#1
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        test domask,ina wc
        addx acca,acca
        mov frqa,#0
        test domask,ina wc
        addx acca,acca
        wrbyte acca,accb
        djnz ctr2,#rbyte        
        mov frqa,#0
        neg phsb,#1
        call #sendiohi
        call #sendiohi
        or outa,csmask
        mov rw_status,ctr2        
        mov frqa,#0 
        or outa,csmask
        neg phsb,#1
        call #sendiohi
'pause
        mov acca,#511
        add acca,cnt
        waitcnt acca,#0
SD_Read_ret
        ret          

sendio
        rol phsb,#24
sendiohi
        mov ctr,#8
        neg frqa,#1
        mov accb,#0
bit     mov phsa,#8
        test domask,ina wc
        addx accb,accb        
        rol phsb,#1
        djnz ctr,#bit
sendio_ret
sendiohi_ret
        ret
checktime
        mov duration,cnt
        sub duration,starttime
        cmp duration,clockfreq wc
checktime_ret
  if_c  ret             
        neg duration,#13
        and duration,C_FFFF
        or  duration,#1
        'wrlong duration,boxDat1Ret         
        
        mov frqa,#0        
        or outa,csmask
        neg phsb,#1
        call #sendiohi
'pause
        mov acca,#511
        add acca,cnt
        waitcnt acca,#0
        jmp #finalERR
        
cmd
        andn outa,csmask
        neg phsb,#1
        call #sendiohi
        mov phsb,cmdo
        add phsb,#$40
        call #sendio
        mov phsb,cmdp
        shl phsb,#9
        call #sendiohi
        call #sendiohi
        call #sendiohi
        call #sendiohi
        mov phsb,#$95
        call #sendio
readresp
        neg phsb,#1
        call #sendiohi
        call #checktime
        cmp accb,#$ff wz
   if_z jmp #readresp 
cmd_ret
readresp_ret
        ret
busy
        neg phsb,#1
        call #sendiohi
        call #checktime
        cmp accb,#$0 wz
   if_z jmp #busy
busy_ret
        ret
               
' Change these constants for different hardware configuration
do        long   6
clk       long   7
di        long   8
cs        long   9

rw_status long   0  
nco       long   $1000_0000
hifreq    long   $e0_00_00_00
freq      long   $20_00_00_00
clockfreq long   80_000_000
onek      long   1000
sector    long   512
domask    long   0
csmask    long   0
acca      long   0
accb      long   0
cmdo      long   0
cmdp      long   0
parptr    long   0
parptr2   long   0
ctr       long   0
ctr2      long   0
starttime long   0
duration  long   0

command            long 0
sectorAddress      long 0
memoryPtr          long 0
firstDataSector    long 0
com                long 0
count              long 0
tmp                long 0
ptr_hold           long 0

C_AA55             long $AA55
C_FFFF             long $FFFF
tmp6               long 0
tmp4               long 0
tmp5               long 0
t1                 long 0
t2                 long 0
t3                 long 0

lastAdr  fit
        