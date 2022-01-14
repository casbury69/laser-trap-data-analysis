#pragma rtGlobals=1		// Use modern global access method.
//----------------------------------------------------------------------------------------------------------------------------//
// Gets binary data from *.dat file, and calibration info from corresponding *.cal file
macro LoadDam1Data()
  DoWindow /k table0 // gets rid of that pesky table0 window
  GBLoadWave/N/I/W=4/V=1/T={4,4}
  Newpath/O datapath,S_path
  rename wave0,bX
  rename wave1,bY
  rename wave2,sX
  rename wave3,sY
  LoadWave/T/P=datapath/W S_fileName[0,strlen(S_fileName) - 5]+".cal" // for new IGOR text *.cal files 
  make/n=1/t filename // added by chip 15_1202 so that filename is stored for later use
  filename = S_fileName[0,strlen(S_fileName) - 5] // added by chip 15_1202 so that filename is stored for later use
  endcomments[0]=noCRLF(endcomments[0])
  SetScale/P x 0,(1/datarate[0]),"", bX,bY,sX,sY
  if ( (exists("kx")!=1)  || (exists("ky")!=1) )
    print "Stiffnesses not found in *.cal file."
    print "For data recorded before 06_0508 enter: kx=0.0260 and ky=0.0162."
    print "For data recorded on or after 06_0508 enter: kx=0.0053 and ky=0.0031."
    print "For data recorded on or after 06_0620 enter: kx=0.0244 and ky=0.0134."
    AskForStiffnesses()
  endif
  print endcomments[0]
end
//----------------------------------------------------------------------------------------------------------------------------//
// Gets binary data from *.dat file, and calibration info from corresponding *.cal file
// This version modified on 2/12/2019 by Chip to load data from the new dual-trap force-clamp program
macro LoadDualTrapData()
  DoWindow /k table0 // gets rid of that pesky table0 window
  GBLoadWave/N/I/W=9/V=1/T={4,4}
  Newpath/O datapath,S_path
  rename wave0,bX1
  rename wave1,bY1
  rename wave2,bX2
  rename wave3,bY2
  rename wave4,sX1
  rename wave5,sX2
  rename wave6,tip1
  rename wave7,tip2
  rename wave8,flags
  LoadWave/T/P=datapath/W S_fileName[0,strlen(S_fileName) - 5]+".cal" // for new IGOR text *.cal files 
  make/n=1/t filename // added by chip 15_1202 so that filename is stored for later use
  filename = S_fileName[0,strlen(S_fileName) - 5] // added by chip 15_1202 so that filename is stored for later use
  endcomments[0]=noCRLF(endcomments[0])
  SetScale/P x 0,(1/datarate[0]),"", bX1,bY1,bX2,bY2,sX1,sX2,tip1,tip2,flags
  GetFlags()
  print endcomments[0]
end
//----------------------------------------------------------------------------------------------------------------------------//
// Extracts three boolean flag waves, trigger1, trigger2, and coupled, from the single, 16-bit,
// floating point wave in which they're embedded by the double-trap force clamp program.
macro GetFlags()
  duplicate/o flags,trigger1,trigger2,coupled,remainder
  coupled = trunc(round(flags)/4)
  remainder = mod(round(flags),4)
  trigger2 = trunc(remainder/2)
  remainder = mod(remainder,2)
  trigger1 = trunc(remainder)
  killwaves flags
end
//----------------------------------------------------------------------------------------------------------------------------//
macro AskForStiffnesses(xstiffness,ystiffness)
  variable xstiffness=0.0260,ystiffness=0.0162 // default suggested stiffnesses
  // stiffnesses prior to lowering trap stiffness on 06_0508 were:  kx=0.0260    ky=0.0162  
  // stiffnesses after lowering trap stiffness on 06_0508 were:  kx=0.0053    ky=0.0031
  // stiffnesses after increasing trap stiffness on 06_0620 were:  kx=0.0244    ky=0.0134
  Prompt xstiffness, "kx (pN/nm)"
  Prompt ystiffness, "ky (pN/nm)"
  Make/O/N=1 kx,ky
  kx[0]=xstiffness
  ky[0]=ystiffness
end
//---------------------------------------------------------------------------------------------------------------------------------------------------//
// Removes CR and LF characters from any string.  I use this to clean up the finalcomments[0] string.
// This version replaces the CR and LF characters with spaces.
function/s noCRLF(astring)
  string astring
  string newstring=""
  variable i
  for(i=0;i<strlen(astring);i+=1)
    if(char2num(astring[i])!=10 && char2num(astring[i])!=13)
      newstring+=astring[i]
    else
      newstring+=" "
    endif
  endfor
  return newstring
end
//----------------------------------------------------------------------------------------------------------------------------//
// The following routines are junk and are not currently in use....
//----------------------------------------------------------------------------------------------------------------------------//
// Crops one event from the data, between users chosen cursor positions
macro CropEvent(num)
  variable num
  variable dummy
  duplicate/O/R=[pcsr(A),pcsr(B)] sX,$("sX"+num2str(num))
  duplicate/O/R=[pcsr(A),pcsr(B)] sY,$("sY"+num2str(num))
  duplicate/O/R=[pcsr(A),pcsr(B)] sY,$("sR"+num2str(num))  
  dummy=$("sX"+num2str(num))[0]
  $("sX"+num2str(num))-=dummy
  dummy=$("sY"+num2str(num))[0]
  $("sY"+num2str(num))-=dummy
  $("sR"+num2str(num))=sign(atan2(($("sY"+num2str(num))),($("sX"+num2str(num)))))*sqrt(($("sX"+num2str(num)))^2+($("sY"+num2str(num)))^2)
  duplicate/O/R=[pcsr(A),pcsr(B)] bX,$("bX"+num2str(num))
  duplicate/O/R=[pcsr(A),pcsr(B)] bY,$("bY"+num2str(num))
  Display $("sR"+num2str(num))
  AppendToGraph/R $("bX"+num2str(num)),$("bY"+num2str(num))
  ModifyGraph rgb($("bX"+num2str(num)))=(65280,48896,48896),rgb($("bY"+num2str(num)))=(48896,52992,65280)
  ModifyGraph mode($("bX"+num2str(num)))=2,mode($("bY"+num2str(num)))=2
  ModifyGraph rgb($("sR"+num2str(num)))=(0,0,0)
  ModifyGraph standoff=0
  ModifyGraph fSize=8
  Label left "Stage position (nm)";DelayUpdate
  Label bottom "Time (s)";DelayUpdate
  Label right "Bead-trap separation (nm)"
  ModifyGraph tick=2,mirror(bottom)=1
  Display $("bY"+num2str(num)) vs $("bX"+num2str(num))
  SetAxis left -150,150 ;DelayUpdate
  SetAxis bottom -150,150 
  AppendToGraph $("sY"+num2str(num)) vs $("sX"+num2str(num))
  ModifyGraph mode=2,rgb($("bY"+num2str(num)))=(43520,43520,43520),rgb($("sY"+num2str(num)))=(0,0,0)
  ModifyGraph grid=1
  Label left "\\Z08Y position (nm)";DelayUpdate
  Label bottom "X position (nm)"
end
//----------------------------------------------------------------------------------------------------------------------------//
// Routine to automatically estimate the force during clamping.
function GetForce(cX,cY,ucX,ucY)
  wave cX, cY // coordinates of bead during clamping
  wave ucX, ucY // coordinates of bead resting in trap without external load (nominally 0,0)
  variable Fx,eFx,Fy,eFy // components of force in X and Y, and error estimates for these components
  variable F,eF // total force magnitude and error estimate
  variable theta // apparent angle of force (to be compared with actual angle of stage movement)
  duplicate /R=[pcsr(A),pcsr(B)] cX,sepX // temporary waves for the bead-trap separation
  duplicate /R=[pcsr(A),pcsr(B)] cY,sepY
  wavestats /Q ucX
  sepX-=V_avg // correction for error in rest position
  wavestats /Q ucY
  sepY-=V_avg
  wavestats /Q sepX
  Fx = V_avg
  eFx = V_sdev
  Fx*=0.0260
  eFx*=0.0260 // force uncertainty assuming error arises from thermal motion (no contribution from stiffness estimate)
  wavestats /Q sepY
  Fy = V_avg
  eFy = V_sdev
  Fy*=0.0162
  eFy*=0.0162 // force uncertainty assuming error arises from thermal motion (no contribution from stiffness estimate)
  F = sqrt(Fx^2+Fy^2)
  eF = sqrt((Fx*eFx/F)^2+(Fy*eFy/F)^2) // error in F using propogation of errors formula (no kidding)
  theta = atan2(Fy,Fx)*180/PI
  Print "Fx = "+num2str(Fx)+" ± "+num2str(eFx)+" pN"
  Print "Fy = "+num2str(Fy)+" ± "+num2str(eFy)+" pN"
  Print "|F| = "+num2str(F)+" ± "+num2str(eF)+" pN"
  Print "theta = "+num2str(theta)+" degrees"
  Killwaves sepX,sepY
end
