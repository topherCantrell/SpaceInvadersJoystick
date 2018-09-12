'' SpaceInvaders ROM as SPIN
'
PUB getData
  return @_program_

DAT

_program_

{

LD   A,$AA
LD   ($3000),A
HALT


}

  byte  $00
  
  byte  $3E, $AA
  byte  $32, $00, $30
  
  byte  $76