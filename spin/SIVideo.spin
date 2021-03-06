CON

  fntsc         = 3_579_545     'NTSC color frequency
  lntsc         = 3640          'NTSC color cycles per line * 16
  sntsc         = 624           'NTSC color cycles per sync * 16     

' Modified driver from the propeller library.
'
' The SpaceInvaders screen is 256x224 organized as 1-pixel-per-bit.
' 256*224/8 = 7168 bytes
'
' The display here is only 256x208. That means 16 rows must be dropped -- probably 8 at the
' top and 8 at the bottom. The starting point of rendering is passed as a parameter to this
' driver. Thus the caller can initialize the driver to any offset address.
  
PUB start(basePin,screenMemoryPtr, displayRowNumPtr)


' TODO make this generic. Right now there are only two know uses: PEGS Joystick and demo board

  if(basePin==12) ' Demo board
        C_VCFG := %0_11_1_1_1_000_00000000000_001_0_11110000 '(group 001 is P8..P15. Mask P12..P15)
        C_DIRA := %00000000_00000000_11110000_00000000       '(Outputs on P12..P15)
  else            ' Joystick
        C_VCFG := %0_10_1_1_1_000_00000000000_011_0_00000111 '(group 011 is P24..P31. Mask P24..P26)
        C_DIRA := %00001111_00000000_00000000_00000000       '(Outputs on P24..P27)

  screenMemoryP   := screenMemoryPtr
  displayRowNumP  := displayRowNumPtr

  return cognew(@entry,0) 
   
DAT   

         org

entry   
                 
         ' Hardware parameters
         mov       vcfg,C_VCFG
         mov       dira,C_DIRA
         mov       ctra,C_CTRA
         mov       frqa,C_FRQA                                   
'
' Superfield
'
superfield
         mov       phaseflip,phasemask 'set phase flip         
                        
         mov       x,vinv              'do invisible back porch lines
black    call      #hsync              'do hsync
         waitvid   burst,sync_high2    'do black 
         djnz      x,#black            'another black line?
                        
         mov       x,vb                'do visible back porch lines
         sub       x,#4                ' Do all blank ...           
         call      #blank_lines        ' ... lines                   
        
         mov       y,_vt               ' Number of rows (26*8 = 208)         
 
         mov       onum,#0             'Drawing row 0
         mov       screen,screenMemoryP'Start of raster buffer
                                                         
renderLine                   

         wrbyte    onum,displayRowNumP ' Tell everyone which row we are rendering         
                        
         call      #hsync              'do hsync

         mov       vscl,hb             'do visible back porch pixels
         xor       tile,colortable
         waitvid   tile,#0

         mov       x,_ht               ' Number of words per row (32/2 = 16)
         mov       vscl,hx             ' set horizontal expand

doPixels

         rdword    tile,screen
         add       screen,#2
         mov       pixels,#0
         shl       tile,#16

          ' ponmlkji_hgfedcba => ppoonnmmllkkjjii_hhggffeeddccbbaa 

         ' No time to execute a DJNZ between shifts. Do it manually.
                  
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
         rcl       tile,#1 wc
         rcl       pixels,#2
                 
         mov       tile,phaseflip      ' Phase flip
         xor       tile,colortable     ' Color table                                                  
         waitvid   tile,pixels         ' Shift out the next 16 pixels (2 tiles + sprites) 

         djnz      x,#doPixels         ' Do all 32 bytes (16 words) on this row    

         mov       vscl,hf             'do visible front porch pixels
         mov       tile,phaseflip      '
         xor       tile,colortable     '
         waitvid   tile,#0             '

         add       onum,#1             ' Next row number

         djnz      y,#renderLine       ' Do all 26*8 rows 
                       
         mov       x,vf         
         call      #blank_lines        

         call      #hsync              'if required, do short line
         mov       vscl,hrest          '
         waitvid   burst,sync_high2    '
         
         call      #vsync_high         'do high vsync pulses

         movs      vsync1,#sync_low1   'do low vsync pulses
         movs      vsync2,#sync_low2   '
         call      #vsync_low          '

         call      #vsync_high         'do high vsync pulses

         mov       vscl,hhalf          'if odd frame, do half line
         waitvid   burst,sync_high2

         jmp       #superField         ' Do next frame
         
'
' Blank lines
'
blank_lines
         call      #hsync              'do hsync
         xor       tile,colortable     'do background
         waitvid   tile,#0
         djnz      x,#blank_lines
blank_lines_ret
         ret
         
'
' Horizontal sync
'
hsync
         mov       vscl,sync_scale1    'do hsync       
         mov       tile,phaseflip
         xor       tile,burst
         waitvid   tile,sync_normal
         mov       vscl,hvis           'setup in case blank line
         mov       tile,phaseflip
hsync_ret
         ret
         
'
' Vertical sync
'
vsync_high
         movs      vsync1,#sync_high1      'vertical sync
         movs      vsync2,#sync_high2               
vsync_low
         mov       x,vrep
vsyncx   mov       vscl,sync_scale1
vsync1   waitvid   burst,sync_high1
         mov       vscl,sync_scale2
vsync2   waitvid   burst,sync_high2
         djnz      x,#vsyncx
vsync_low_ret
vsync_high_ret
         ret
         
phaseflip               long    $00000000
phasemask               long    $F0F0F0F0
sync_high1              long    %0101010101010101010101_101010_0101
sync_high2              long    %01010101010101010101010101010101       'used for black
sync_low1               long    %1010101010101010101010101010_0101
sync_low2               long    %01_101010101010101010101010101010     

C_CLKFREQ  long $04_C4_B4_00           ' Hardcoded clock frequency

' These are filled in before COG is started (based on I/O pins)
C_VCFG     long 0_0
C_DIRA     long 0_0
             
C_CTRA     long $07_00_00_00 'Internal video mode
C_FRQA     long $16_E8_B9_FC 'Used by video

_ht                     long     32/2      
_vt                     long     26*8    
t1                      long     0     
x                       long     0                
y                       long     0
hf                      long     228
hb                      long     228
vf                      long     17
vb                      long     18
hx                      long     $A0_A0  
screen                  long     0
offsets                 long     0
tile                    long     0
pixels                  long     0              

hvis                    long     lntsc - sntsc      
hrest                   long     lntsc / 2 - sntsc
hhalf                   long     lntsc / 2
vvis                    long     243
vinv                    long     10
vrep                    long     6
burst                   long     $02_8A

fcolor                  long     fntsc                      
sync_scale1             long     sntsc >> 4 << 12 + sntsc
sync_scale2             long     67 << 12 + lntsc / 2 - sntsc
sync_normal             long     %0101_00000000_01_10101010101010_0101


colortable        long  $05_6C_BC_02   ' Color scheme

onum              long  0              ' Running count of row number

screenMemoryP     long  $0_0           ' Pointer to screen raster
displayRowNumP    long  $0_0           ' Pointer to row-number output
          