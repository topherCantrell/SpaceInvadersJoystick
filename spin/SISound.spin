'
'   1  bit  0=UFO (repeats)        
'   2  bit  1=Shot                 
'   4  bit  2=Flash (player die)   
'   8  bit  3=Invader die          
'  16  bit  4=Extended play        
'  32  bit  5=
'  64  bit  6=
' 128  bit  7=
'
' 256  bit  8=Fleet movement 1     
' 512  bit  9=Fleet movement 2     
'1024  bit 10=Fleet movement 3     
'2048  bit 11=Fleet movement 4     
'4096  bit 12=UFO Hit              
'
'
' Channel 1: Fleet movement, Flash
' Channel 2: Shot, Invader die
' Channel 3: UFO, UFO Hit, Extended play
' 

CON _clkmode = xtal1 + pll16x
    _xinfreq = 5_000_000    
    playRate = 21 'Hz
                                     
VAR

  long voiceAFunction
  long voiceAInit
  
  long voiceBFunction
  long voiceBInit
  
  long voiceCFunction
  long voiceCInit
  
  long ufoTimer
  long ufoValue
  long extCnt

  long flashTimer
  long fleetTimer

  long fleetTone

  long shotTimer
  long invTimer
 

OBJ

  SID : "SIDCog"
  
PUB Main(soundValuePtr,leftPin,rightPin) | time, cmd, lastCmd
 
  SID.start(leftPin,rightPin)            'Start the emulated SID chip in one cog 
  
  SID.setVolume(15)                      'Set volume to max

  voiceAFunction := 0
  fleetTone := 0
  voiceAInit :=0
  
  voiceBFunction := 0
  voiceBInit :=0
  
  voiceCFunction := 0
  voiceCInit :=0

  cmd :=0                                 

  time := cnt
  repeat
    waitcnt(time+= 75_000)               ' Wait for next 10ms tick
    processVoiceA
    processVoiceB
    processVoiceC
    lastCmd := cmd
    cmd := long[soundValuePtr]
    processComA(cmd, lastCmd)
    processComB(cmd, lastCmd)
    processComC(cmd, lastCmd) 

'------------------------------------------------------------------------------
' Voice A: Fleet or player-die

PUB endVoiceA
  SID.noteOff(0)
  voiceAFunction := 0
  voiceAInit := 0
  
  return
  
PUB processComA(cmd, lastCmd) | newNum, t
' Channel 1: Fleet movement, Flash
'  bit  2=Flash (player die)   
'  bit  8=Fleet movement 1     
'  bit  9=Fleet movement 2     
'  bit 10=Fleet movement 3     
'  bit 11=Fleet movement 4

   t := cmd & 4
   if t<>0
     ' The player-die bit is set
     if(voiceAFunction<>2)
       ' It wasn't set last time ... init on the transition
       voiceAFunction := 2
       voiceAInit := 0       
     else
       ' Still set ... let it run (it will stop when done)
   else    

     ' Get the fleet tone number
     newNum := cmd>>8
     newNum := newNum & 15

     if(newNum==0)
       ' Turn fleet off
       endVoiceA       

     elseif(newNum<>fleetTone)
       ' Fleet tone has changed ... init a new tone
       voiceAFunction :=1
       voiceAInit := 0
       fleetTone := newNum 
    
PUB processVoiceA
' Fleet or player-explosion
  if voiceAFunction<>0

    if voiceAInit==0
      if voiceAFunction==1
        initFleet
      else
        initFlash
    else
      if voiceAFunction==1
        runFleet
      else
        runFlash

PUB initFleet
  SID.setWaveform(0,2)                   'Set waveform type on channel to Square wave
  SID.setPWM(0,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(0,  2,6,0,2)                'Set Envelope on channel
  voiceAInit := 1
  fleetTimer := 80
  
PUB runFleet             
  if fleetTimer==80
    if fleetTone==1
      SID.noteOn(0, 4000)
    elseif fleetTone==2
      SID.noteOn(0, 3600)
    elseif fleetTone==4
      SID.noteOn(0, 3200)
    else
      SID.noteOn(0, 3000)
    --fleetTimer
  elseif fleetTimer==0
    SID.noteOff(0)
    endVoiceA
  
PUB initFlash
  SID.setWaveform(0,3)                   'Set waveform type on channel to Square wave
  SID.setPWM(0,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(0,  3,9,0,0)                'Set Envelope on channel
  voiceAInit := 1
  flashTimer := 2000
  
PUB runFlash
  if flashTimer==2000
    SID.noteOn(0,3000)
    --flashTimer
  elseif flashTimer==0
    SID.noteOff(0)
    endVoiceA

  return

'------------------------------------------------------------------------------
' Voice B: Player-shot or invader-die

PUB endVoiceB
  SID.noteOff(1)
  voiceBFunction := 0
  voiceBInit := 0
  return
  
PUB processComB(cmd, lastCmd) | t, u
' Channel 2: Shot, Invader die
'  bit  1=Shot   
'  bit  3=Invader die

    t := cmd & 2
    u := lastCmd & 2

    if u<>0
      t := 0
    
    if t<>0
       voiceBFunction := 1
       voiceBInit := 0
       return
        
    t := cmd & 8
    u := lastCmd & 8

    if u<>0
      t := 0
      
    if t<>0
      voiceBFunction := 2
      voiceBInit := 0

    return
    
PUB processVoiceB
' Shot or invader-die
  if voiceBFunction==0
    return

  if voiceBInit==0
    if voiceBFunction==1
      initShot
    else
      initInvDie
  else
    if voiceBFunction==1
      runShot
    else
      runInvDie
    
  return   

PUB initShot
  SID.setWaveform(1,0)                   'Set waveform type on channel to Square wave
  SID.setPWM(1,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(1,  2,6,0,4)                'Set Envelope on channel
  voiceBInit := 1
  shotTimer := 200              
  return
  
PUB runShot
         
  if shotTimer==200
    SID.noteOn(1,100000)
  elseif shotTimer==0
    SID.noteOff(1)
    'endVoiceB

  --shotTimer
  
  return
  
PUB initInvDie
  SID.setWaveform(1,3)                   'Set waveform type on channel to Square wave
  SID.setPWM(1,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(1,  2,4,0,0)                'Set Envelope on channel
  voiceBInit := 1
  invTimer := 200        
  return
  
PUB runInvDie

  if invTimer==200
    SID.noteOn(1,100000)
  elseif invTimer==0
    SID.noteOff(1)
    'endVoiceB

  --invTimer
  
  return



'------------------------------------------------------------------------------
' Voice C: UFO, UFO-Hit, and ExtendedPlay

PUB endVoiceC
  SID.noteOff(2)
  voiceCFunction := 0
  voiceCInit := 0
  return
  
PUB processComC(cmd, lastCmd) | newCmd, t
' Channel 3: UFO, UFO Hit, Extended play
'  bit  0=UFO (repeats) 
'  bit  4=Extended play 
'  bit 12=UFO Hit

    newCmd := 0
    t := cmd & 1
    if t<>0
      newCmd := 1
        
    t := cmd & 4096
    if t<>0
      newCmd := 2

    t := cmd & 16
    if t<>0
      newCmd := 3
          
    if newCmd==0
      voiceCFunction := 0
      return

    if newCmd<>voiceCFunction
      voiceCFunction := newCmd
      voiceCInit := 0
      return

PUB processVoiceC
' UFO or UFO-hit or Extended-play
  if voiceCFunction==0
    endVoiceC
    return

  if voiceCInit==0
    if voiceCFunction==1
      initUFO
    elseif voiceCFunction==2
      initUFOHit
    else
      initExtendedPlay
  else
    if voiceCFunction==1
      runUFO
    elseif voiceCFunction==2
      runUFOHit
    else
      runExtendedPlay

  return

PUB initExtendedPlay
  SID.setWaveform(2,1)                   'Set waveform type on channel to Square wave
  SID.setPWM(2,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(2,  2,15,0,0)                'Set Envelope on channel
  voiceCInit:=1
  ufoValue:=0
  ufoTimer:=0
  extCnt:=8
  return

PUB runExtendedPlay
  if ufoTimer<>0
    --ufoTimer
    return

  if extCnt==0
    endVoiceC
    return

  --extCnt

  ufoTimer := 250

  if ufoValue==0
    SID.noteOn(2,18000)
    ufoValue := 1
  else
    SID.noteOff(2)
    ufoValue := 0
    
  return
  
PUB initUFOHit
  SID.setWaveform(2,2)                   'Set waveform type on channel to Square wave
  SID.setPWM(2,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(2,  1,8,5,0)                'Set Envelope on channel
  voiceCInit:=1
  ufoValue:=0
  ufoTimer:=0
  return

PUB runUFOHit
  if ufoTimer<>0
    --ufoTimer
    return

  ufoTimer := 30

  if ufoValue==0
    SID.noteOn(2,5000)
    ufoValue := 1
  else
    SID.noteOff(2)
    ufoValue := 0 
  return           
  
PUB initUFO
  SID.setWaveform(2,0)                   'Set waveform type on channel to Square wave
  SID.setPWM(2,1928)                     'Set the pulse width on channel to 47:53
  SID.setADSR(2,  6,15,0,0)                'Set Envelope on channel
  'SID.setWaveform(2,2)                   'Set waveform type on channel to Square wave
  'SID.setPWM(2,1928)                     'Set the pulse width on channel to 47:53
  'SID.setADSR(2,  1,8,5,0)                'Set Envelope on channel
  voiceCInit:=1
  ufoValue:=0
  ufoTimer:=0
  return

PUB runUFO
  if ufoTimer<>0
    --ufoTimer
    return

  ufoTimer := 30

  if ufoValue==0
    SID.noteOn(2,5000)
    ufoValue := 1
  else
    SID.noteOff(2)
    ufoValue := 0
    
  return  
  