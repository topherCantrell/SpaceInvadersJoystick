{{

CPU CONTROL

   The CPU can be controlled externally for debugging. The "command" register holds the status
   of the CPU. A value of 0 means the CPU is running. A value of $1 means the CPU has finished
   halting.

   Write a $2 to the command to request a halt. The CPU will copy its internal registers to
   RAM and transition to the $1 (halted state).

   Write a $10 to the command to request a restore. The CPU will load its internal registers
   from RAM and transition to the 0 (running) state.

   Write a $12 to the command to request a step. The CPU will load its internal registers from
   RAM, execute one cycle, and enter the HALT sequence.

   Opcodes like HALT will initiate the halt sequence from within the running CPU program.

MEMORY AND IO

   Most of the time the CPU will read and write from plain global RAM. But some memory-mapped
   hardware and I/O addresses require a command/response pattern.

   With a triggered read the address and command (1 for memory or 3 for I/O) are written to the
   parameter block in memory. The CPU waits on the handler to clear the command and then reads
   the value.

   With a triggered write the address, value, and command (2 for memory or 4 for I/O) are written
   to the parameter block in memory. The CPU waits for the handler to clear the command before
   continuing.

   Memory decoding is divided into 4 areas each with its own offset in global memory. Each area
   begins where the last left off. The first area begins at 0000 and the last ends at FFFF.
   [0000..A) offsetA
   [A..B)    offsetB
   [B..C)    offsetC
   [C..FFFF] offsetD

   An offset of 0 for the area means that area is "triggered". Accesses go through the longer
   handshaking process.

PARAMETERS

   memOffset  offset to CPU's address in global RAM
   
   command     $0 CPU is running
               $1 CPU is halted
               $2 Request halt (writes registers to memory)
              $10 Request restore+run (loads registers from memory and runs)
              $12 Request restore+step (loads registers from memory and runs one cycle)

   interrupt  0=done, y_xx=interrupt request (CPU specific)

   dump       Pointer to mem where registers are dumped/loaded 

   ioCommand  0=done, 1=read mem, 2=write mem, 3=read io, 4=write io
   ioAddress  memory/io address to read/write
   ioValue    read value or value to write   

}}

CON ' COMMON TO ALL CPUs
 
  ' Shift amounts for decode-table parameters
  dec_param    = 26
  dec_reg_v1   = 26  
  dec_reg_v3   = 29

  ' Shift amounts for decode-table vectors  
  dec_v1  =  0
  dec_v2  =  9
  dec_v3  = 18  

CON ' 8080 SPECIFIC

  ' 8080 flags
  sign_bit   =   %10000000
  zero_bit   =   %01000000
  '              %00100000
  half_bit   =   %00010000
  '              %00001000
  parity_bit =   %00000100
  subtract_bit = %00000010
  carry_bit  =   %00000001

  ' Shift amounts for each flag that can be used
  ' in a conditional flow instruction
  bn_carry =  0
  bn_parity = 2
  bn_zero =   6
  bn_sign =   7

  ' Port commands
  io_cmd_out = $01 
  io_cmd_in  = $02
  
  ' Register offset within the COG memory  
  dec_a = 0
  dec_f = 1
  dec_b = 2 
  dec_c = 3
  dec_d = 4
  dec_e = 5
  dec_h = 6
  dec_l = 7
  
PUB start(memArea0, memOffset0, memArea1, memOffset1,memArea2, memOffset2,memArea3, memOffset3,ioPtr, cpuPtr) | i, cogNumber

  memArea[0] := memArea0  
  memOfs[0]  := memOffset0
  memArea[1] := memArea1  
  memOfs[1]  := memOffset1
  memArea[2] := memArea2  
  memOfs[2]  := memOffset2
  memArea[3] := memArea3  
  memOfs[3]  := memOffset3

  ioCmdP     := ioPtr
  ioAddressP := ioPtr+4
  ioValueP   := ioPtr+8

  commandP   := cpuPtr
  interruptP := cpuPtr+4
  'cycleCntP  := cpuPtr+8
  dumpP      := cpuPtr+12  

  object_base := @@0 ' Note where this code starts in global memory   
  
  return cognew(@enter, 0) 'Start emulator in a new COG

DAT
         org
enter
         add       dispatch_tab_addr, object_base ' ##Fixup this object offset to HUB real address
         add       daa_tab_addr, object_base      ' 8080 specific DAA table
         jmp       #mainLoop                      ' ##Start main loop (vector 1 commands have to be first in COG memory)

' -------------------------------------------------------------------------------------------------
' --------------- Start of Vector3 Functions ------------------------------------------------------

' Data Destination Functions (these are vector3 functions that always end with fetching next)
' These appear early in the code because the upper bit of the vector's address is used as a
' 6th bit for the parameter field. The upper bit must be 0.

'' Push the PC and set its value to data_16
push_set_pc
         mov       t2,data_16          ' Hold the new PC value
         mov       data_16,regPC       ' Value to push is the current PC
         call      #push               ' Push current PC on stack
         mov       regPC,t2            ' Set PC to new value           
nop_v3   jmp       #mainLoop           ' Done

'' Set the PC to data_16
set_pc   mov       regPC,data_16       ' Set the new PC value
         jmp       #mainLoop           ' Done

'' Set the SP to data_16
set_sp   mov       regSP,data_16       ' Set the new SP value
         jmp       #mainLoop           ' Done

'' Set R8 to data_8
setR8    movd      sr8a, paramRegD     ' Set the address of the destination register
         nop                           ' Required gap before access
sr8a     mov       0,data_8            ' Copy data_8 to target register   
         jmp       #mainLoop           ' Done

'' Set R16 to data_16             
setR16   movd      sr16a,paramRegD     ' Address of upper register of pair
         movd      sr16b,paramRegD     ' Address of upper register of pair (again)
         add       paramRegD,#1        ' Next register is lower
         movd      sr16c,paramRegD     ' Address of lower register of pair
         movd      sr16d,paramRegD     ' Address of lower register of pair (again)
sr16a    mov       0,data_16           ' Set upper register
sr16b    shr       0,#8                ' Shift upper byte into place
sr16c    mov       0,data_16           ' Set lower register
sr16d    and       0,#$FF              ' Mask lower byte into place          
         jmp       #mainLoop           ' Done  

'' Write data_8 to [R16]
setBR16  movs      sbr16a,paramRegD    ' Address of upper register of pair
         add       paramRegD,#1        ' Next register is lower
         movs      sbr16b,paramRegD    ' Address of lower register of pair
sbr16a   mov       address, 0          ' Get the upper byte
         shl       address,#8          ' Shift into place
sbr16b   or        address, 0          ' OR in the lower byte
         call      #memWrite           ' Write the value from data_8            
         jmp       #mainLoop           ' Done

'' Write data_8 to [code-immediate-word]
set_byte_caddr  
         mov       t1,data_8           ' Hold the value to write
         mov       address,regPC       ' Read address ...  
         call      #read_memory_word   ' ... from immediate code memory         
         add       regPC, #2           ' Skip over the address in code
         mov       address,data_16     ' The address we just read
         mov       data_8,t1           ' The original value 
         call      #memWrite           ' Write the value
         jmp       #mainLoop           ' Done

'' Write data_16 to [code-immediate-word]
set_word_caddr
         mov       t1,data_16          ' Hold the value to write
         mov       address, regPC      ' Read address ...
         call      #read_memory_word   ' ... from immediate code memory
         add       regPC, #2           ' Skip over the address in code
         mov       address,data_16     ' The address we just read
         mov       data_16,t1          ' The original value
         call      #write_memory_word  ' Write the value
         jmp       #mainLoop           ' Done

'' Pop PC from the stack
pop_pc   call      #pop                ' Pop the value from the stack
         mov       regPC,data_16       ' Move value to PC           
         jmp       #mainLoop           ' Done

'' Push PC and set PC to param*8
set_pc_rst
         mov       data_16,param       ' Get RST number from param field
         shl       data_16,#3          ' *8
         jmp       #push_set_pc        ' Push the PC and set the PC     

'' Push R16         
pushReg  movs      pra,paramRegD       ' Address of upper register of pair
         add       paramRegD,#1        ' Next register is lower
         movs      prb,paramRegD       ' Address of lower register of pair
pra      mov       data_16,0           ' Get the upper byte
         shl       data_16,#8          ' Shift it into position
prb      or        data_16,0           ' OR in the lower byte
         call      #push               ' Push the value              
         jmp       #mainLoop           ' Done

'' Pop R16
popReg   call      #pop                ' Pop the value from the stack
         jmp       #setR16             ' Set the target register pair

         fit       $1_00               ' Make sure upper (9th) bit of address is 0

' -------------------------------------------------------------------------------------------------
' --------------- Start of Vector2 Functions ------------------------------------------------------

flagSetOrSkip
         mov       t1,#1               ' Bit number 0
         shl       t1,param            ' Set desired bit number
         and       f_reg,t1 wz, nr     ' Test the desired flag
   if_z  jmp       #mainLoop           ' If it isn't set then skip last vector
nop_v2   jmp       #vect_3             ' Process the last vector (flag is set)

flagClearOrSkip
         mov       t1,#1               ' Bit number 0
         shl       t1,param            ' Set desired bit number         
         and       f_reg,t1 wz, nr     ' Test the desired flag
   if_nz jmp       #mainLoop           ' If it is set then skip last vector
         jmp       #vect_3             ' Process the last vector (flag is not set)

enableInt
         mov       intEnable,param     ' Set the enabled flag (1 or 0 from param)
         jmp       #vect_3             ' Process the last vector

ioIn     mov       address,data_8
         mov       tmp,#3
         call      #ioRead
         mov       a_reg,data_8
         jmp       #vect_3        

ioOut    mov       address,data_8
         mov       data_8,a_reg
         mov       tmp,#4
         call      #ioWrite
         jmp       #vect_3      

opCCF    and       f_reg,#carry_bit wz,nr             ' Check the original C bit status
         andn      f_reg,#(half_bit | carry_bit)      ' Copy original C ...
 if_z    or        f_reg,#half_bit                    ' ... to half-carry (and clear C for the moment)
 if_nz   or        f_reg,#carry_bit                   ' Flip the original C value
         jmp       #vect_3                            ' Done
         
opSCF    andn      f_reg,#(half_bit | subtract_bit)   ' Clear half-carry and subtract bit
         or        f_reg,#carry_bit                   ' Set the carry-bit
         jmp       #vect_3                            ' Done

opOR     or        data_8,a_reg wc, wz ' OR A with data_8 (C = result parity, Z = result zero)
         mov       f_reg,#0            ' Clear all flags (will set the right ones)
         jmp       #logicFlags         ' Set the flags

opXOR    xor       data_8,a_reg wc, wz ' XOR A with data_8 (C = result parity, Z = result zero)
         mov       f_reg,#0            ' Clear all flags (will set the right ones)
         jmp       #logicFlags         ' Set the flags

opAND    and       data_8,a_reg wc, wz ' AND A with data_8 (C = result parity, Z = result zero)       
         mov       f_reg,#half_bit     ' Clear all flags, set half-carry
         
logicFlags
  if_c   or        f_reg,#parity_bit   ' Copy parity flag from C
  if_z   or        f_reg,#zero_bit     ' Copy zero flag from Z        
         and       data_8,#sign_bit wz, nr ' Copy sign ...
  if_nz  or        f_reg,#sign_bit     ' ... bit from result
         jmp       #vect_3             ' Done

opINC16  add       data_16,#1          ' Increment the 16-bit value (no flags)
         and       data_16,C_FFFF      ' Wrap if needed
         jmp       #vect_3             ' Done
         
opDEC16  sub       data_16,#1          ' Decrement the 16-bit value (no flags)
         and       data_16,C_FFFF      ' Wrap if needed
         jmp       #vect_3             ' Done

opCPL    xor       a_reg,#%11111111                   ' Toggle all bits in the A register
         mov       f_reg,#(subtract_bit | half_bit)   ' Set subtract and half-carry bits
         jmp       #vect_3                            ' Done

opEXDEHL mov       t1,d_reg            ' Exchange ...
         mov       d_reg,h_reg         ' ... D ...
         mov       h_reg,t1            ' ... and H
         mov       t1,e_reg            ' Exchange ...
         mov       e_reg,l_reg         ' ... E ...
         mov       l_reg,t1            ' ... and L
         jmp       #vect_3             ' Done

opEXSPHL call      #pop                ' Get the value from the stack
         mov       t1,data_16          ' Hold the value
         mov       data_16,h_reg       ' Get value ...
         shl       data_16,#8          ' ... of ...
         or        data_16,l_reg       ' ... HL
         call      #push               ' Push HL on stack
         mov       h_reg,t1            ' Set H ...
         shr       h_reg,#8            ' ... to upper value
         mov       l_reg,t1            ' Set L ...
         and       l_reg,#255          ' ... to lower value
         jmp       #vect_3             ' Done

opDAA

' Lots of room here if you drop DAA support
       
         mov       data_16,f_reg            ' Keep flags ...         
         and       data_16,#%00010011       ' ... carry, subtract, half         
         mov       paramRegS,a_reg          ' Get upper ...         
         shr       paramRegS,#4             ' ... nibble
         mov       paramRegD,a_reg          ' Get lowr ...
         and       paramRegD,#%1111         ' ... nibble
         
         mov       address,daa_tab_addr     ' DAA table in shared RAM
         mov       data_8,#13               ' 13 entries in the table

opdaaA   rdbyte    t1,address               ' Read first byte                                        
         mov       t7,t1                    ' Hold new-carry
         and       t1,#%0_1111111           ' Mask off new-carry
         shr       t7,#7                    ' Into carry-bit position
         
         add       address,#1               ' Read rest of ...
         rdbyte    t2,address               ' ... table ...
         add       address,#1               ' ... entry ...
         rdbyte    t3,address               ' ... ...
         add       address,#1               ' ... ...
         rdbyte    t4,address               ' ... ...
         add       address,#1               ' ... ...
         rdbyte    t5,address               ' ... ...
         add       address,#1               ' ... ...
         rdbyte    t6,address               ' ... ...
         add       address,#1               ' ...

         cmp       data_16,t1 wz            ' Are the flags right?
  if_nz  jmp       #opdaaN                  ' No ... next entry  

  
         cmp       paramRegS,t2 wz, wc      ' See if ...
  if_b   jmp       #opdaaN                  ' ... upper ...
         cmp       paramRegS,t3 wz, wc      ' ... and lower ...
  if_a   jmp       #opdaaN                  ' ... nibbles ...
         cmp       paramRegD,t4 wz, wc      ' ... match ...
         
  if_b   jmp       #opdaaN                  ' ... entry's ranges ...
         cmp       paramRegD,t5 wz, wc      ' ... ...
  if_a   jmp       #opdaaN                  ' ...
         
         add       a_reg,t6                 ' Adjust by given amount
         and       a_reg,#$FF               ' Make to 8 bits
         andn      f_reg,#carry_bit         ' Set carry ...         
         or        f_reg,t7                 ' ... to new value
         ' TODO parity is affected here
         jmp       #vect_3                  ' Done         

opdaaN   djnz      data_8,#opdaaA           ' Do all in the table

         'mov       a_reg,#$11
         jmp       #vect_3                  ' No entry ... do nothing

opSBC    mov       t1,a_reg                 ' Hold this as "original value" for HV flags  
         mov       t2,a_reg                 ' Do the math on t2 so we leave A alone
         sub       t2,data_8                ' Subtract data_8 from A (t2 = A - data)
         and       f_reg,#carry_bit wz,nr   ' If borrow is set ...
  if_nz  sub       t2,#1                    ' ... take one more away
         mov       f_reg,#subtract_bit      ' Clear all flags (will set shortly)
         jmp       #opSCom                  ' Common flag setting with SUB command

opDEC    mov       t1,data_8                ' Hold this as "original value" for HV flags
         mov       t2,data_8                ' Do the math on t2 so we leave A alone
         sub       t2,#1                    ' t2 = data - 1
         mov       f_reg,#subtract_bit      ' Clear all flags (will set shortly)
         jmp       #opSCom2                 ' Common flag setting with SBC and SUB

opINC    mov       t1,data_8                ' Hold this as "original value" for HV flags
         mov       t2,data_8                ' Do the math on t2 so we leave A alone
         add       t2,#1                    ' t2 = data + 1
         mov       f_reg,#0                 ' Clear all flags (will set shortly)
         jmp       #opACom2                 ' Common flag setting with ADC and ADD

opADC    mov       t1,a_reg                 ' Hold this as "original value" for HV flags
         mov       t2,a_reg                 ' Do the math on t2 so we leave A alone
         add       t2,data_8                ' t2 = A + data
         and       f_reg,#carry_bit wz,nr   ' If the carr is set ...
  if_nz  add       t2,#1                    ' ... then add one more
         mov       f_reg,#0                 ' Clear all flags (will set shortly)
         jmp       #opACom                  ' Common flag setting with ADD command
         
opADD    mov       t1,a_reg                 ' Hold this as "original value" for HV flags
         mov       t2,a_reg                 ' Do the math on t2 so we leave A alone
         add       t2,data_8                ' t2 = A + data
         mov       f_reg,#0                 ' Clear all flags (will set shortly)

opACom   cmp       t2,#$FF wc, wz           ' Set carry if ...
  if_a   or        f_reg,#carry_bit         ' ... result is > 255
  
opACom2  ' If the lower nibble is less after then there must have been a carry
         mov      t3,t1                     ' Lower nibble ...
         and      t3,#%1111                 ' ... of starting value
         mov      t4,t2                     ' Lower nibble ...
         and      t4,#%1111                 ' ... of final value
         cmp      t4,t3 wc, wz              ' Compare before and after
  if_b   or       f_reg,#half_bit           ' If after is lower then there was a carry

         'TODO Set v flag (overflow)                                                  

opASCom
         and       t2,#%1000_0000  nr, wz   ' Copy sign bit ...
  if_nz  or        f_reg,#sign_bit          ' ... from result to flags
         and       t2,#%1111_1111  nr, wz   ' If result is zero ...
   if_z  or        f_reg,#zero_bit          ' ... then set zero flag
         and       t2,#255                  ' Limit result to a byte
         mov       data_8,t2                ' Result back to data_8 (again, A is untouched)
         jmp       #vect_3                  ' Set S and Z. Common to all ADD, SUB, INC, and DEC                

opSUB    mov       t1,a_reg                 ' Hold this as "original value" for HV flags
         mov       t2,a_reg                 ' Do the math on t2 so we leave A alone
         sub       t2,data_8                ' t2 = A - data
         mov       f_reg,#subtract_bit      ' Clear all flags (will set shortly)

opSCom   cmp       t2,#$FF wc, wz           ' Set borrow ...
  if_a   or        f_reg,#carry_bit         ' ... if reslut is > 255
  
opSCom2  ' If the lower nibble is greater after then there must have been a borrow 
         mov      t3,t1                     ' Lower nibble ...
         and      t3,#%1111                 ' ... of starting value
         mov      t4,t2                     ' Lower nibble ...
         and      t4,#%1111                 ' ... of final value
         cmp      t4,t3 wc, wz              ' Compare before and after
   if_a  or       f_reg,#half_bit           ' If after is higher then there was a borrow
  
         'TODO Set v flag (overflow)              

opSCom3  ' Set sz flags

         jmp       #opASCom            ' Sign, zero, wrap, and move result

opADD16  mov       t1,h_reg            ' Use t1 ...
         shl       t1,#8               ' ... as original ...
         or        t1,l_reg            ' ... HL value
         mov       t2,t1               ' Do the math of t2 so we leave A
         add       t2,data_16          ' t2 = HL - data 
         mov       f_reg,#0            ' Clear all flags (will set shortly)

         ' TODO set H
                 
         cmp       t2,C_FFFF wz, wc    ' If result is > 65535 ...
  if_z   or        f_reg,#carry_bit    ' ... then set carry
         and       t2,C_FFFF           ' Wrap result if needed
         mov       data_16,t2          ' Result back to data_16 (again, HL is unaffected)
         jmp       #vect_3             ' Done                    
         
opRLA    shl       a_reg,#1                           ' Shift A to the left
         and       f_reg,#carry_bit nr, wz            ' Copy carry ...
  if_nz  or        a_reg,#1                           ' ... to right most bit
         and       a_reg,#%1_00000000 nr, wz          ' Copy bit ...
         andn      f_reg,#carry_bit                   ' ... shifted out of left ...
  if_nz  or        f_reg,#carry_bit                   ' ... to the carry bit
         and       a_reg,#255                         ' Limit to 1 byte
         andn      f_reg,#(subtract_bit+half_bit)     ' Reset subtract and half-carry
         jmp       #vect_3                            ' Done

opRRA    mov       t1,a_reg                           ' Hold lowest bit
         shr       a_reg,#1                           ' Shift A to the right
         and       f_reg,#carry_bit nr, wz            ' Copy carry ...
  if_nz  or        a_reg,#%1000_0000                  ' ... to left most bit
         andn      f_reg,#carry_bit                   ' Copy bit ...
         and       t1,#1 nr, wz                       ' ... shifted out of right ...
  if_nz  or        f_reg,#carry_bit                   ' ... to the carry bit
         andn      f_reg,#(subtract_bit+half_bit)     ' Reset subtract and half-carry
         jmp       #vect_3                            ' Done
         
opRLCA   andn      f_reg,#(subtract_bit+half_bit+carry_bit) ' Reset subtract and half-carry
         shl       a_reg,#1                           ' Shift A to the left
         and       a_reg,#%1_00000000 nr, wz          ' Copy bit ...
         andn      a_reg,#carry_bit                   ' ... shifted out of left ...
  if_nz  or        a_reg,#1                           ' ... to right most bit ...
  if_nz  or        f_reg,#carry_bit                   ' ... and carry
         and       a_reg,#255                         ' Limit to 1 byte                  
         jmp       #vect_3                            ' Done

opRRCA   andn      f_reg,#(subtract_bit+half_bit+carry_bit) ' Reset subtract and half-carry
         mov       t1,a_reg                           ' Hold lowest bit
         shr       a_reg,#1                           ' Shift A to the right
         andn      f_reg,#carry_bit                   ' Copy bit ...
         and       t1,#1 nr, wz                       ' ... shifted out of right ...
  if_nz  or        a_reg,#%10000000                   ' ... to left most bit ...
  if_nz  or        f_reg,#carry_bit                   ' ... and carry         
         jmp       #vect_3                            ' Done 


' -------------------------------------------------------------------------------------------------
' --------------- Start of Vector1 Functions ------------------------------------------------------

get_sp   mov       data_16, regSP      ' Get the value of the SP register
nop_v1   jmp       #vect_2             ' Done           
         
getR8    movs      gr8b,paramRegS      ' Address of the 8-bit register
         nop                           ' Required gap before access
gr8b     mov       data_8, 0           ' Get the value of the desired register
         jmp       #vect_2             ' Done  
         
getR16   movs      gr16b, paramRegS    ' Address of upper register of pair
         add       paramRegS,#1        ' Next regsiter is lower
         movs      gr16c, paramRegS    ' Address of lower register of pair         
gr16b    mov       data_16,0           ' Get the upper byte
         shl       data_16,#8          ' Shift it into place
gr16c    or        data_16,0           ' OR in the lower byte
         jmp       #vect_2             ' Done  

get_code_byte
         mov       address, regPC      ' Program counter points to immediate
         call      #memRead            ' Read byte from code
         add       regPC, #1           ' Bump the PC
         jmp       #vect_2             ' Done     

get_code_word  
         mov       address, regPC      ' Program counter points to immediate
         call      #read_memory_word   ' Read word from code
         add       regPC, #2           ' Bump the PC
         jmp       #vect_2             ' Done      

getBR16  movs      gbr16a, paramRegS   ' Address of upper register of pair
         add       paramRegS,#1        ' Next register is lower
         movs      gbr16b, paramRegS   ' Address of lower register or pair        
gbr16a   mov       address, 0          ' Get the upper byte
         shl       address, #8         ' Shift it into place
gbr16b   or        address, 0          ' OR in the lower byte
getbdone call      #memRead            ' Read the byte from 8080 ram
         jmp       #vect_2             ' Done    
         
get_byte_caddr
         mov       address, regPC      ' PC points to the immediate
         call      #read_memory_word   ' Read the address from the code
         add       regPC, #2           ' Bump the PC
         mov       address,data_16     ' Immediate value is the address
         jmp       #getbdone           ' Read byte and done

get_word_caddr
         mov       address, regPC      ' PC points to the immediate
         call      #read_memory_word   ' Read the address from the code
         add       regPC, #2           ' Bump the PC
         mov       address,data_16     ' Immediate value is the address
         call      #read_memory_word   ' Get the word
         jmp       #vect_2             ' Done

get_word_hl
         mov       address, h_reg      ' H is the upper register of pair
         shl       address,#8          ' Shift it into place
         or        address, l_reg      ' OR in the lower byte
         call      #read_memory_word   ' Read the word from 8080 ram
         jmp       #vect_2             ' Done

' --------------- End of Vector Functions --------------------------------------------------------
' -------------------------------------------------------------------------------------------------

' Word access routines for 8080

read_memory_word
         call      #memRead            ' 8080 Read the low byte
         mov       data_16, data_8     ' 8080 Hold the low byte
         add       address, #1         ' 8080 Next address
         call      #memReadA           ' 8080 Read the high byte (same area)
         shl       data_8,#8           ' 8080 Shift high byte into position
         or        data_16, data_8     ' 8080 Merge high and low byte         
read_memory_word_ret                   ' 8080 
         ret                           ' 8080 Done

write_memory_word
         mov       data_8, data_16     ' 8080 Get the low byte
         call      #memWrite           ' 8080 Write the low byte
         shr       data_8, #8          ' 8080 Get the high byte
         add       address, #1         ' 8080 Next address
         call      #memWriteA          ' 8080 Write the high byte (same area)
write_memory_word_ret                  ' 8080 
         ret                           ' 8080 Done

' 8080 stacks grows by decrement. Only words are pushed/popped (not bytes).
' The stack pointer points to the the last word pushed (the next word to pop).

push     sub        regSP,#2           ' Make room for the next word
         mov        address,regSP      ' SP is the address
         call       #write_memory_word ' Write data_16 to stack
push_ret ret                           ' Done
                                       
pop      mov       address,regSP       ' SP is the address
         call      #read_memory_word   ' Read data_16 from stack
         add       regSP,#2            ' Drop word from the stack
pop_ret  ret                           ' Done    


mainLoop
         rdlong    tmp,commandP wz     ' ##Get the status (check for 0)
  if_z   jmp       #irrReq             ' ##Running free ... do next cycle

' Check/handle HALT sequence
 
         cmp       tmp,#$2 wz          ' ##Halt request?
  if_nz  jmp       #checkRestore       ' ##No ... try restore         

un_v1
un_v2
un_v3
halt_v2  mov       tmp3,#DSTART        ' ##Where the dump area begins
         mov       address,dumpP       ' ##Where the dump goes
         mov       tmp2,#DEND-DSTART   ' ##Size of dump area 

mhalt    movd      md1,tmp3            ' ##Pointer to next to go
         add       tmp3,#1             ' ##Bump pointer
md1      wrlong    0,address           ' ##Write dump value
         add       address,#4          ' ##Next in RAM
         djnz      tmp2,#mhalt         ' ##Do all

         mov       tmp,#1              ' ##Note HALT ...
         wrlong    tmp,commandP        ' ##... complete
         jmp       #mainLoop           ' ##Wait

' Check/handle RESTORE sequence
         
checkRestore
         cmp       tmp,#$10 wz, wc     ' ##Is this a restore request?
  if_b   jmp       #mainLoop           ' ##No ... ignore it                                             

         mov       tmp3,#DSTART        ' ##Where the dump area begins
         mov       address,dumpP       ' ##Where the dump comes from
         mov       tmp2,#DEND-DSTART   ' ##Size of dump area             

mrest    movd      md2,tmp3            ' ##Pointer to next to load
         add       tmp3,#1             ' ##Bump pointer
md2      rdlong    0,address           ' ##Read dump value
         add       address,#4          ' ##Next in RAM
         djnz      tmp2,#mrest         ' ##Do all

         and       tmp,#%0000_1111     ' ##Note RESTORE ...                                 
         wrlong    tmp,commandP        ' ##... complete (or STEP) 

irrReq        
         rdlong    t3,interruptP wz    ' 8080 Read the interrupt status  
         mov       t2,t3               ' 8080 Get ...
         shr       t2,#12              ' 8080 ... interrupt type
         cmp       t2,#1 wz            ' 8080 1 means IRQ?
  if_nz  jmp       #int1               ' 8080 No ... move on
         cmp       intEnable,#1 wz     ' 8080 IRQ enabled?
  if_nz  jmp       #int1               ' 8080 No ... move on        
         jmp       #int3               ' 8080 Take the interrupt  
int1     cmp       t2,#2 wz            ' 8080 2 means NMI?
  if_nz  jmp       #decode             ' 8080 No ... move on     
int3     mov       t2,#0               ' 8080 Remove the ...
         wrlong    t2,interruptP       ' 8080 ... interrupt request for next pass
         mov       data_16,regPC       ' 8080 Push the ...
         call      #push               ' 8080 ... program counter
         mov       regPC,t3            ' 8080 Get the address
         and       regPC,#$FF          ' 8080 Mask off the command

decode
         'add       cycles,#1                ' ##Bump the count ...
         'wrlong    cycles,cycleCntP         ' ##... of decode cycles

         mov       address, regPC           ' ##PC to address parameter
         call      #memRead                 ' ##Read the instruction byte from memory 
         add       regPC, #1                ' ##Increment the program counter

         mov       tmp, data_8              ' ##Dispatch table look up
         shl       tmp, #2                  ' ##Mul by 4 to get a LONG offset into the table
         add       tmp, dispatch_tab_addr   ' ##Index into table
         rdlong    vector, tmp              ' ##Read the instruction handlers vectors (3 per long)
         
         mov       param,vector             ' ##The param field ...
         shr       param,#26                ' ##... is top 6 bits
         
         mov       paramRegS,param          ' 8080 The lower three ...
         and       paramRegS,#%111          ' 8080 ... bits of param ...
         add       paramRegS,#a_reg         ' 8080 ... could be src register
         mov       paramRegD,param          ' 8080 The upper three ...
         shr       paramRegD,#3             ' 8080 ... bits of param ...
         add       paramRegD,#a_reg         ' 8080... could be dst register
               
         jmp       vector                   ' ##Jump to the instruction handler  
         
vect_2   shr       vector, #9               ' ##Shift to 2nd vector
         jmp       vector                   ' ##Execute get functions

vect_3   shr       vector, #9               ' ##Shift to 3rd vector
         and       vector, #$FF             ' ##The upper bit here is part of the params
         jmp       vector                   ' ##Execute set functions

getMemArea 
         mov       tmp,memOfs+3             ' ##Try upper area (3)
         cmp       address,memArea+3 wz, wc ' ##Address in reange of upper?
   if_ae jmp       #:found                  ' ##Yes ... found it
         mov       tmp,memOfs+2             ' ##Try area 2
         cmp       address,memArea+2 wz,wc  ' ##Address in area 2?
   if_ae jmp       #:found                  ' ##Yes ... found it
         mov       tmp,memOfs+1             ' ##Try area 1
         cmp       address,memArea+1 wz,wc  ' ##Address in area 1?
   if_ae jmp       #:found                  ' ##Yes ... found it
         mov       tmp,memOfs               ' ##Use area 0
:found   add       address,tmp              ' ##Add the memory offset (if any)
         cmp       tmp,#0 wz                ' ##A zero would be an indication of "triggered"          
getMemArea_ret                              ' ##
         ret                                ' ##Done

memRead  call      #getMemArea              ' ##Decode the memory address
memReadA                                    ' ##Entry for 2-in-a-row
  if_nz  jmp       #memReadPlain            ' ##Not "triggered" ... do plain
         mov       tmp,#1                   ' ##CMD=1 (memory read)
         call      #ioRead                  ' ##Process the fetch as an I/O command
         jmp       #memRead_ret             ' ##Done
memReadPlain                                ' ##
         rdbyte    data_8,address           ' ##Get the byte                         
memRead_ret                                 ' ##
memReadA_ret                                ' ##
         ret                                ' ##Done

memWrite call      #getMemArea              ' ##Decode the memory address
memWriteA                                   ' ##Entry for 2-in-a-row           
  if_nz  jmp       #memWritePlain           ' ##Not "triggered" ... do plain
         mov       tmp,#2                   ' ##CMD=2 (memory write)
         call      #ioWrite                 ' ##Process the store as an I/O command
         jmp       #memWrite_ret            ' ##Done
memWritePlain                               ' ##
        wrbyte    data_8,address            ' ##Write the byte
memWrite_ret                                ' ##
memWriteA_ret                               ' ##
         ret                                ' ##Done
                                            
ioRead   wrlong    address,ioAddressP       ' ##Write the IO address                                         
         wrlong    tmp,ioCmdP               ' ##Start the IO process (command passed in)
:wait    rdlong    tmp,ioCmdP wz            ' ##Wait for handler ...
   if_nz jmp       #:wait                   ' ##... to complete
         rdlong    data_8,ioValueP          ' ##Get the IO value
ioRead_ret                                  ' ##
         ret                                ' ##Done

ioWrite  wrlong    address,ioAddressP       ' ##Write the IO address
         wrlong    data_8,ioValueP          ' ##Write the IO value
         wrlong    tmp,ioCmdP               ' ##Start the IO process (command passed in)
:wait    rdlong    tmp,ioCmdP wz            ' ##Wait for handler ...
  if_nz  jmp       #:wait                   ' ##... to complete
ioWrite_ret                                 ' ##
         ret                                ' ##Done   

DSTART            ' ##These values are copied/restored in a HALT/RESTORE sequence

a_reg    long $0  ' 8080 A
f_reg    long $0  ' 8080 F
b_reg    long $0  ' 8080 B 
c_reg    long $0  ' 8080 C
d_reg    long $0  ' 8080 D
e_reg    long $0  ' 8080 E
h_reg    long $0  ' 8080 H
l_reg    long $0  ' 8080 L
'
regSP    long $0  ' ##Stack pointer (most CPUs have one)
regPC    long $0  ' ##Program counter (most CPUs have one)

DEND              ' ##End of the HALT/RESTORE values 

intEnable      long $0         ' 8080 interrupt-enabled flag
C_FFFF         long $FFFF      ' 8080 FFFF constant
paramRegS      long $0         ' 8080 Source-parameter field
paramRegD      long $0         ' 8080 Destination-parameter field
t1             long $0         ' 8080 Temporary
t2             long $0         ' 8080 Temporary 
t3             long $0         ' 8080 Temporary 
t4             long $0         ' 8080 Temporary 
t5             long $0         ' 8080 Temporary 
t6             long $0         ' 8080 Temporary 
t7             long $0         ' 8080 Temporary 
data_16        long $0         ' 8080 16-bit values
daa_tab_addr   long @daaTable  ' 8080 Pointer to DAA table in global RAM (adjusted at entry)   
    
tmp            long $0         ' ##Temporary
tmp2           long $0         ' ##Temporary
tmp3           long $0         ' ##Temporary
vector         long $0         ' ##Value from dispatch table (kept across all vector processing) 
param          long $0         ' ##Parameter data from vector (kept across all vector processing)
address        long $0         ' ##Memory-access address
data_8         long $0         ' ##8-bit value
'cycles         long $0         ' ##Count of CPU cycles  (comment out if not needed)

' Memory-decode areas and offsets
memArea        long  $0_0, $0_0, $0_0, $0_0 ' ##Four memory areas                    
memOfs         long  $0_0, $0_0, $0_0, $0_0 ' ##Offsets to global RAM

' External parameter pointers                    
ioCmdP         long  $0_0                   ' ##Triggered memory access command
ioAddressP     long  $0_0                   ' ##Triggered memory address
ioValueP       long  $0_0                   ' ##Triggered memory value
commandP       long  $0_0                   ' ##CPU command/status
interruptP     long  $0_0                   ' ##CPU interrupt request     
dumpP          long  $0_0                   ' ##Register-dump area

'cycleCntP      long  $0_0                   ' ##Where to write cycle-count when it changes (comment out if not needed)

dispatch_tab_addr long @dispatch_table      ' ##Pointer to dispatch_table in global RAM (adjusted at entry)
object_base       long $0_0                 ' ##Location of object in shared ram

'---------------------------------------------------------------------------------------------------------
dispatch_table ' 8080 Opcodes        

{00} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (nop_v3<<dec_v3) '  00       NOP                  --------
{01} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2) + (setR16<<dec_v3) + (dec_b<<dec_reg_v3) '  01wlwm   LD BC,w              --------
{02} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_b<<dec_reg_v3) + (dec_a<<dec_reg_v1) '  02       LD (BC),A            --------
{03} long (getR16<<dec_v1) + (opINC16<<dec_v2) + (setR16<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  03       INC BC               --------
{04} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_b<<dec_reg_v3)     '  04       INC B                sz-h-v0-
{05} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_b<<dec_reg_v3)     '  05       DEC B                sz-u-v1-
{06} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v3) '  06bb     LD B,b               --------
{07} long (nop_v1<<dec_v1) + (opRLCA<<dec_v2) + (nop_v3<<dec_v3)   '  07       RLCA                 ---0--0x
{08} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{09} long (getR16<<dec_v1) + (opADD16<<dec_v2) + (setR16<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  09       ADD HL,BC            ---H--0C
{0A} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  0A       LD A,(BC)            --------
{0B} long (getR16<<dec_v1) + (opDEC16<<dec_v2) + (setR16<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  0B       DEC BC               --------
{0C} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_c<<dec_reg_v3)     '  0C       INC C                sz-h-v0-
{0D} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_c<<dec_reg_v3)     '  0D       DEC C                sz-u-v1-
{0E} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v3) '  0Ebb     LD C,b               --------
{0F} long (nop_v1<<dec_v1) + (opRRCA<<dec_v2) +  (nop_v3<<dec_v3)  '  0F       RRCA                 ---0--0x
{10} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{11} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2) + (setR16<<dec_v3) + (dec_d<<dec_reg_v3) '  11wlwm   LD DE,w              --------
{12} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_d<<dec_reg_v3) + (dec_a<<dec_reg_v1) '  12       LD (DE),A            --------
{13} long (getR16<<dec_v1) + (opINC16<<dec_v2) + (setR16<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  13       INC DE               --------
{14} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_d<<dec_reg_v3)     '  14       INC D                sz-h-v0-
{15} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_d<<dec_reg_v3)     '  15       DEC D                sz-u-v1-
{16} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v3) '  16bb     LD D,b               --------
{17} long (nop_v1<<dec_v1) + (opRLA<<dec_v2) + (nop_v3<<dec_v3)    '  17       RLA                  ---0--0x
{18} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{19} long (getR16<<dec_v1) + (opADD16<<dec_v2) + (setR16<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  19       ADD HL,DE            ---H--0C
{1A} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  1A       LD A,(DE)            --------
{1B} long (getR16<<dec_v1) + (opDEC16<<dec_v2) + (setR16<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  1B       DEC DE               --------
{1C} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_e<<dec_reg_v3)     '  1C       INC E                sz-h-v0-
{1D} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_e<<dec_reg_v3)     '  1D       DEC E                sz-u-v1-
{1E} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v3) '  1Ebb     LD E,b               --------
{1F} long (nop_v1<<dec_v1) + (opRRA<<dec_v2) + (nop_v3<<dec_v3)    '  1F       RRA                  ---0--0x
{20} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{21} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2) + (setR16<<dec_v3) + (dec_h<<dec_reg_v3) '  21wlwm   LD HL,w              --------
{22} long (getR16<<dec_v1) + (nop_v2<<dec_v2) + (set_word_caddr<<dec_v3) + (dec_h<<dec_reg_v1)  '  22tltm   LD (t),HL            --------
{23} long (getR16<<dec_v1) + (opINC16<<dec_v2) + (setR16<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  23       INC HL               --------
{24} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3)     '  24       INC H                sz-h-v0-
{25} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3)     '  25       DEC H                sz-u-v1-
{26} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v3) '  26bb     LD H,b               --------
{27} long (nop_v1<<dec_v1) + (OPDAA<<dec_v2) + (nop_v3<<dec_v3)    '  27       DAA                  xxxxxxxx
{28} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{29} long (getR16<<dec_v1) + (opADD16<<dec_v2) + (setR16<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  29       ADD HL,HL            ---H--0C
{2A} long (get_word_caddr<<dec_v1) + (nop_v2<<dec_v2) + (setR16<<dec_v3) + (dec_h<<dec_reg_v3)  '  2Atltm   LD HL,(t)            --------
{2B} long (getR16<<dec_v1) + (opDEC16<<dec_v2) + (setR16<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  2B       DEC HL               --------
{2C} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_l<<dec_reg_v3)     '  2C       INC L                sz-h-v0-
{2D} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_l<<dec_reg_v3)     '  2D       DEC L                sz-u-v1-
{2E} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v3) '  2Ebb     LD L,b               --------
{2F} long (nop_v1<<dec_v1) + (opCPL<<dec_v2) + (nop_v3<<dec_v3)    '  2F       CPL                  ---1--1-
{30} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{31} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2) + (set_sp<<dec_v3)                       '  31wlwm   LD SP,w              --------
{32} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (set_byte_caddr<<dec_v3)  + (dec_a<<dec_reg_v1)  '  32tltm   LD (t),A             --------
{33} long (get_sp<<dec_v1) + (opINC16<<dec_v2) + (set_sp<<dec_v3)                                             '  33       INC SP               --------
{34} long (getBR16<<dec_v1) + (opINC<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  34       INC (HL)             sz-h-v0-
{35} long (getBR16<<dec_v1) + (opDEC<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  35       DEC (HL)             sz-u-v1-
{36} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3)  '  36bb     LD (HL),b            --------
{37} long (nop_v1<<dec_v1) + (opSCF<<dec_v2) + (nop_v3<<dec_v3) '  37       SCF                  ---0--01                                     
{38} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{39} long (get_sp) + (opADD16<<dec_v2) + (setR16<<dec_v3)                               + (dec_h<<dec_reg_v3) '  39       ADD HL,SP            ---H--0C
{3A} long (get_byte_caddr<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3)  + (dec_a<<dec_reg_v3)  '  3Atltm   LD A,(t)             --------
{3B} long (get_sp<<dec_v1) + (opDEC16<<dec_v2) + (set_sp<<dec_v3)                                             '  3B       DEC SP               --------
{3C} long (getR8<<dec_v1) + (opINC<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3)     '  3C       INC A                sz-h-v0-
{3D} long (getR8<<dec_v1) + (opDEC<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3)     '  3D       DEC A                sz-u-v1-
{3E} long (get_code_byte<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v3) '  3Ebb     LD A,b               --------
{3F} long (nop_v1<<dec_v1) + (opCCF<<dec_v2) + (nop_v3<<dec_v3) '  3F       CCF                  ---x--0x
{40} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  40       LD B,B               --------
{41} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  41       LD B,C               --------
{42} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  42       LD B,D               --------
{43} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  43       LD B,E               --------
{44} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  44       LD B,H               --------
{45} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  45       LD B,L               --------
{46} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  46       LD B,(HL)            --------
{47} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_b<<dec_reg_v3) '  47       LD B,A               --------
{48} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  48       LD C,B               --------
{49} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  49       LD C,C               --------
{4A} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4A       LD C,D               --------
{4B} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4B       LD C,E               --------
{4C} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4C       LD C,H               --------
{4D} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4D       LD C,L               --------
{4E} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4E       LD C,(HL)            --------
{4F} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_c<<dec_reg_v3) '  4F       LD C,A               --------
{50} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  50       LD D,B               --------
{51} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  51       LD D,C               --------
{52} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  52       LD D,D               --------
{53} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  53       LD D,E               --------
{54} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  54       LD D,H               --------
{55} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  55       LD D,L               --------
{56} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  56       LD D,(HL)            --------
{57} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_d<<dec_reg_v3) '  57       LD D,A               --------
{58} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  58       LD E,B               --------
{59} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  59       LD E,C               --------
{5A} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5A       LD E,D               --------
{5B} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5B       LD E,E               --------
{5C} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5C       LD E,H               --------
{5D} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5D       LD E,L               --------
{5E} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5E       LD E,(HL)            --------
{5F} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_e<<dec_reg_v3) '  5F       LD E,A               --------
{60} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  60       LD H,B               --------
{61} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  61       LD H,C               --------
{62} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  62       LD H,D               --------
{63} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  63       LD H,E               --------
{64} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  64       LD H,H               --------
{65} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  65       LD H,L               --------
{66} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  66       LD H,(HL)            --------
{67} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_h<<dec_reg_v3) '  67       LD H,A               --------
{68} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  68       LD L,B               --------
{69} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  69       LD L,C               --------
{6A} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6A       LD L,D               --------
{6B} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6B       LD L,E               --------
{6C} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6C       LD L,H               --------
{6D} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6D       LD L,L               --------
{6E} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6E       LD L,(HL)            --------
{6F} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_l<<dec_reg_v3) '  6F       LD L,A               --------
{70} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_b<<dec_reg_v1) '  70       LD (HL),B            --------
{71} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_c<<dec_reg_v1) '  71       LD (HL),C            --------
{72} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_d<<dec_reg_v1) '  72       LD (HL),D            --------
{73} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_e<<dec_reg_v1) '  73       LD (HL),E            --------
{74} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_h<<dec_reg_v1) '  74       LD (HL),H            --------
{75} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_l<<dec_reg_v1) '  75       LD (HL),L            --------
{76} long (nop_v1<<dec_v1)        + (halt_v2<<dec_v2)  + (nop_v3<<dec_v3) '  76       HALT                 xxxxxxxx                 
{77} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setBR16<<dec_v3) + (dec_h<<dec_reg_v3) + (dec_a<<dec_reg_v1) '  77       LD (HL),A            --------
{78} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  78       LD A,B               --------
{79} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  79       LD A,C               --------
{7A} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7A       LD A,D               --------
{7B} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7B       LD A,E               --------
{7C} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7C       LD A,H               --------
{7D} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7D       LD A,L               --------
{7E} long (getBR16<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7E       LD A,(HL)            --------
{7F} long (getR8<<dec_v1) + (nop_v2<<dec_v2) + (setR8<<dec_v3) + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  7F       LD A,A               --------
{80} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  80       ADD A,B              sz-h-v0c
{81} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  81       ADD A,C              sz-h-v0c
{82} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  82       ADD A,D              sz-h-v0c
{83} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  83       ADD A,E              sz-h-v0c
{84} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  84       ADD A,H              sz-h-v0c
{85} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  85       ADD A,L              sz-h-v0c
{86} long (getBR16<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  86       ADD A,(HL)           sz-h-v0c
{87} long (getR8<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  87       ADD A,A              sz-h-v0c
{88} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  88       ADC A,B              sz-h-v0c
{89} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  89       ADC A,C              sz-h-v0c
{8A} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8A       ADC A,D              sz-h-v0c
{8B} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8B       ADC A,E              sz-h-v0c
{8C} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8C       ADC A,H              sz-h-v0c
{8D} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8D       ADC A,L              sz-h-v0c
{8E} long (getBR16<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8E       ADC A,(HL)           sz-h-v0c
{8F} long (getR8<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  8F       ADC A,A              sz-h-v0c
{90} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  90       SUB B                sz-u-v1b
{91} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  91       SUB C                sz-u-v1b
{92} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  92       SUB D                sz-u-v1b
{93} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  93       SUB E                sz-u-v1b
{94} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  94       SUB H                sz-u-v1b
{95} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  95       SUB L                sz-u-v1b
{96} long (getBR16<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  96       SUB (HL)             sz-u-v1b
{97} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  97       SUB A                sz-u-v1b
{98} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  98       SBC B                sz-u-v1b
{99} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  99       SBC C                sz-u-v1b
{9A} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9A       SBC D                sz-u-v1b
{9B} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9B       SBC E                sz-u-v1b
{9C} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9C       SBC H                sz-u-v1b
{9D} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9D       SBC L                sz-u-v1b
{9E} long (getBR16<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9E       SBC (HL)             sz-u-v1b
{9F} long (getR8<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  9F       SBC A                sz-u-v1b
{A0} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A0       AND B                sz-1-p00
{A1} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A1       AND C                sz-1-p00
{A2} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A2       AND D                sz-1-p00
{A3} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A3       AND E                sz-1-p00
{A4} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A4       AND H                sz-1-p00
{A5} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A5       AND L                sz-1-p00
{A6} long (getBR16<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A6       AND (HL)             sz-1-p00
{A7} long (getR8<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A7       AND A                sz-1-p00
{A8} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A8       XOR B                sz-0-p00
{A9} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  A9       XOR C                sz-0-p00
{AA} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AA       XOR D                sz-0-p00
{AB} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AB       XOR E                sz-0-p00
{AC} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AC       XOR H                sz-0-p00
{AD} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AD       XOR L                sz-0-p00
{AE} long (getBR16<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AE       XOR (HL)             sz-0-p00
{AF} long (getR8<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  AF       XOR A                sz-0-p00
{B0} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_b<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B0       OR B                 sz-0-p00
{B1} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_c<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B1       OR C                 sz-0-p00
{B2} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_d<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B2       OR D                 sz-0-p00
{B3} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_e<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B3       OR E                 sz-0-p00
{B4} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B4       OR H                 sz-0-p00
{B5} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_l<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B5       OR L                 sz-0-p00
{B6} long (getBR16<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3) + (dec_h<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B6       OR (HL)              sz-0-p00
{B7} long (getR8<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)   + (dec_a<<dec_reg_v1) + (dec_a<<dec_reg_v3) '  B7       OR A                 sz-0-p00
{B8} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_b<<dec_reg_v1)   '  B8       CP B                 sz-u-v1b
{B9} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_c<<dec_reg_v1)   '  B9       CP C                 sz-u-v1b
{BA} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_d<<dec_reg_v1)   '  BA       CP D                 sz-u-v1b
{BB} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_e<<dec_reg_v1)   '  BB       CP E                 sz-u-v1b
{BC} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_h<<dec_reg_v1)   '  BC       CP H                 sz-u-v1b
{BD} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_l<<dec_reg_v1)   '  BD       CP L                 sz-u-v1b
{BE} long (getBR16<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_h<<dec_reg_v1) '  BE       CP (HL)              sz-u-v1b
{BF} long (getR8<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3) + (dec_a<<dec_reg_v1)   '  BF       CP A                 sz-u-v1b
{C0} long (nop_v1<<dec_v1) + (flagClearOrSkip<<dec_v2) + (pop_pc<<dec_v3) + (bn_zero<<dec_param)    '  C0       RET NZ               --------
{C1} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (popReg<<dec_v3)  + (dec_b<<dec_reg_v3) '  C1       POP BC               --------
{C2} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (set_pc<<dec_v3) + (bn_zero<<dec_param)    '  C2mlmm   JP NZ,m              --------
{C3} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2)          + (set_pc<<dec_v3)                       '  C3mlmm   JP m                 --------
{C4} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (push_set_pc<<dec_v3) + (bn_zero<<dec_param)   '  C4mlmm   CALL NZ,m            --------
{C5} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (pushReg<<dec_v3) + (dec_b<<dec_reg_v3) '  C5       PUSH BC              --------
{C6} long (get_code_byte<<dec_v1) + (opADD<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  C6bb     ADD A,b              sz-h-v0c
{C7} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (0<<dec_param) '  C7       RST $00              --------
{C8} long (nop_v1<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (pop_pc<<dec_v3) + (bn_zero<<dec_param)    '  C8       RET Z                --------
{C9} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2)          + (pop_pc<<dec_v3)                           '  C9       RET                  -------- 
{CA} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (set_pc<<dec_v3) + (bn_zero<<dec_param)    '  CAmlmm   JP Z,m               --------
{CB} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{CC} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (push_set_pc<<dec_v3) + (bn_zero<<dec_param)   '  CCmlmm   CALL Z,m             --------               
{CD} long (get_code_word<<dec_v1) + (nop_v2<<dec_v2)          + (push_set_pc<<dec_v3)                          '  CDmlmm   CALL m               --------
{CE} long (get_code_byte<<dec_v1) + (opADC<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  CEbb     ADC A,b              sz-h-v0c
{CF} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (1<<dec_param) '  CF       RST $08              --------
{D0} long (nop_v1<<dec_v1) + (flagClearOrSkip<<dec_v2) + (pop_pc<<dec_v3) + (bn_carry<<dec_param)   '  D0       RET NC               --------
{D1} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (popReg<<dec_v3)  + (dec_d<<dec_reg_v3) '  D1       POP DE               --------
{D2} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (set_pc<<dec_v3) + (bn_carry<<dec_param)   '  D2mlmm   JP NC,m              --------
{D3} long (get_code_byte<<dec_v1) + (ioOut<<dec_v2) + (nop_v3<<dec_v3) '  D3oo     OUT (o),A            --------
{D4} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (push_set_pc<<dec_v3) + (bn_carry<<dec_param)  '  D4mlmm   CALL NC,m            --------
{D5} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (pushReg<<dec_v3) + (dec_d<<dec_reg_v3) '  D5       PUSH DE              --------
{D6} long (get_code_byte<<dec_v1) + (opSUB<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  D6bb     SUB b                sz-u-v1b
{D7} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (2<<dec_param) '  D7       RST $10              --------
{D8} long (nop_v1<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (pop_pc<<dec_v3) + (bn_carry<<dec_param)   '  D8       RET C                --------
{D9} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{DA} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (set_pc<<dec_v3) + (bn_carry<<dec_param)   '  DAmlmm   JP C,m               --------
{DB} long (get_code_byte<<dec_v1) + (ioIn<<dec_v2)  + (nop_v3<<dec_v3) '  DBoo     IN A,(o)             --------
{DC} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (push_set_pc<<dec_v3) + (bn_carry<<dec_param)  '  DCmlmm   CALL C,m             --------
{DD} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{DE} long (get_code_byte<<dec_v1) + (opSBC<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  DEbb     SBC A,b              sz-u-v1b
{DF} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (3<<dec_param) '  DF       RST $18              --------
{E0} long (nop_v1<<dec_v1) + (flagClearOrSkip<<dec_v2) + (pop_pc<<dec_v3) + (bn_parity<<dec_param)  '  E0       RET PO               --------
{E1} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (popReg<<dec_v3)  + (dec_h<<dec_reg_v3) '  E1       POP HL               --------
{E2} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (set_pc<<dec_v3) + (bn_parity<<dec_param)  '  E2mlmm   JP PO,m              --------
{E3} long (nop_v1<<dec_v1) + (opEXSPHL<<dec_v2) + (nop_v3<<dec_v3) '  E3       EX (SP),HL           --------
{E4} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (push_set_pc<<dec_v3) + (bn_parity<<dec_param) '  E4mlmm   CALL PO,m            --------
{E5} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (pushReg<<dec_v3) + (dec_h<<dec_reg_v3) '  E5       PUSH HL              --------
{E6} long (get_code_byte<<dec_v1) + (opAND<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  E6bb     AND b                sz-1-p00
{E7} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (4<<dec_param) '  E7       RST $20              --------
{E8} long (nop_v1<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (pop_pc<<dec_v3) + (bn_parity<<dec_param)  '  E8       RET PE               --------
{E9} long (getR16<<dec_v1)   + (nop_v2<<dec_v2)          + (set_pc<<dec_v3) + (dec_h<<dec_reg_v1)                      '  E9       JP (HL)              --------
{EA} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (set_pc<<dec_v3) + (bn_parity<<dec_param)  '  EAmlmm   JP PE,m              --------
{EB} long (nop_v1<<dec_v1) + (opEXDEHL<<dec_v2) + (nop_v3<<dec_v3) '  EB       EX DE,HL             --------
{EC} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (push_set_pc<<dec_v3) + (bn_parity<<dec_param) '  ECmlmm   CALL PE,m            --------
{ED} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{EE} long (get_code_byte<<dec_v1) + (opXOR<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  EEbb     XOR b                sz-0-p00
{EF} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (5<<dec_param) '  EF       RST $28              --------
{F0} long (nop_v1<<dec_v1) + (flagClearOrSkip<<dec_v2) + (pop_pc<<dec_v3) + (bn_sign<<dec_param)    '  F0       RET P                --------
{F1} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (popReg<<dec_v3)  + (dec_a<<dec_reg_v3) '  F1       POP AF               xxxxxxxx
{F2} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (set_pc<<dec_v3) + (bn_sign<<dec_param)    '  F2mlmm   JP P,m               --------
{F3} long (nop_v1<<dec_v1) + (enableInt<<dec_v2) + (nop_v3<<dec_v3) + (0<<dec_param) '  F3       DI                   xxxxxxxx
{F4} long (get_code_word<<dec_v1) + (flagClearOrSkip<<dec_v2) + (push_set_pc<<dec_v3) + (bn_sign<<dec_param)   '  F4mlmm   CALL P,m             --------
{F5} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (pushReg<<dec_v3) + (dec_a<<dec_reg_v3) '  F5       PUSH AF              --------
{F6} long (get_code_byte<<dec_v1) + (opOR<<dec_v2) + (setR8<<dec_v3)                 + (dec_a<<dec_reg_v3) '  F6bb     OR b                 sz-0-p00
{F7} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (6<<dec_param) '  F7       RST $30              --------
{F8} long (nop_v1<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (pop_pc<<dec_v3) + (bn_sign<<dec_param)    '  F8       RET M                --------
{F9} long (getR16<<dec_v1) + (nop_v2<<dec_v2) + (set_sp<<dec_v3) + (dec_h<<dec_reg_v1)          '  F9       LD SP,HL             --------
{FA} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (set_pc<<dec_v3) + (bn_sign<<dec_param)    '  FAmlmm   JP M,m               --------
{FB} long (nop_v1<<dec_v1) + (enableInt<<dec_v2) + (nop_v3<<dec_v3) + (1<<dec_param) '  FB       EI                   xxxxxxxx
{FC} long (get_code_word<<dec_v1) + (flagSetOrSkip<<dec_v2)   + (push_set_pc<<dec_v3) + (bn_sign<<dec_param)   '  FCmlmm   CALL M,m             --------
{FD} long (un_v1<<dec_v1) + (un_v2<<dec_v2) + (un_v3<<dec_v3)
{FE} long (get_code_byte<<dec_v1) + (opSUB<<dec_v2) + (nop_v3<<dec_v3)                 '  FEbb     CP b                 sz-u-v1b
{FF} long (nop_v1<<dec_v1) + (nop_v2<<dec_v2) + (set_pc_rst<<dec_v3) + (7<<dec_param) '  FF       RST $38              --------
 
daaTable
'+------------------------------------------------------------------------------+
'|           | C Flag  | HEX value in | H Flag | HEX value in | Number  | C flag|
'| Operation | Before  | upper digit  | Before | lower digit  | added   | After |
'|           | DAA     | (bit 7-4)    | DAA    | (bit 3-0)    | to byte | DAA   |
'|------------------------------------------------------------------------------|
'|           |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
'|   ADD     |    0    |     0-8      |   0    |     A-F      |   06    |   0   |
'|           |    0    |     0-9      |   1    |     0-3      |   06    |   0   |
'|   ADC     |    0    |     A-F      |   0    |     0-9      |   60    |   1   |
'|           |    0    |     9-F      |   0    |     A-F      |   66    |   1   |
'|   INC     |    0    |     A-F      |   1    |     0-3      |   66    |   1   |
'|           |    1    |     0-2      |   0    |     0-9      |   60    |   1   |
'|           |    1    |     0-2      |   0    |     A-F      |   66    |   1   |
'|           |    1    |     0-3      |   1    |     0-3      |   66    |   1   |
'|------------------------------------------------------------------------------|
'|   SUB     |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
'|   SBC     |    0    |     0-8      |   1    |     6-F      |   FA    |   0   |
'|   DEC     |    1    |     7-F      |   0    |     0-9      |   A0    |   1   |
'|   NEG     |    1    |     6-F      |   1    |     6-F      |   9A    |   1   |
'+------------------------------------------------------------------------------+ 

'     C   h  sc      MSN              LSN            add 
byte %0_0000000,     $0,$9      ,     $0,$9      ,   $00
byte %0_0000000,     $0,$8      ,     $A,$F      ,   $06
byte %0_0010000,     $0,$9      ,     $0,$3      ,   $06
byte %1_0000000,     $A,$F      ,     $0,$9      ,   $60
byte %1_0000000,     $9,$F      ,     $A,$F      ,   $66
byte %1_0010000,     $A,$F      ,     $0,$3      ,   $66
byte %1_0000001,     $0,$2      ,     $0,$9      ,   $60
byte %1_0000001,     $0,$2      ,     $A,$F      ,   $66
byte %1_0010001,     $0,$3      ,     $0,$3      ,   $66
byte %0_0000010,     $0,$9      ,     $0,$9      ,   $00 
byte %0_0010010,     $0,$8      ,     $6,$F      ,   $FA 
byte %1_0000011,     $7,$F      ,     $0,$9      ,   $A0    
byte %1_0010011,     $6,$F      ,     $6,$F      ,   $9A                    