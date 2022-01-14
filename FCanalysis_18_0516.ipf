// New version of FC analysis that allows a fourth type of event termination -- interrupted.
// Idea is to designate when an event is temporarily interrupted and then continues in next event, so these can be automatically stitched.
// Based on earlier version ("AndrewFCanalysis") that Andrew and I altered to score pauses in between assembly and disassembly.
// Modifications 16_0210 to allow stall force analysis of open-loop data too (Chip and Megan)
// Modifications 18_0327 to allow pause analysis during ADM (Chip and Aida)
// Modifications 18_0516 to produce an overview plot, showing all the selected events in succession on same scale

#pragma rtGlobals=1		// Use modern global access method.
#include <Power Spectral Density>
#include <Decimation>
//----------------------------------------------------------------------------------------------------------------------------//
macro SetupForceClampAnalysis()
  silent 1; pauseupdate
  variable/G EventNumber,TotalEvents,UnloadedIsMarked,StartedFitting,IgnoreUnloaded,OpenLoop,TermType
  EventNumber = 1   // global variable specifying which event we're looking at now
  TotalEvents = 0  // global variable for total number of events
  UnloadedIsMarked = 0 // global flag for whether user has marked the unloaded bead pos or not
  StartedFitting = 0 // global flag for whether the graphs for fitting and examining individual events have been created or not
  IgnoreUnloaded = 0 // global flag for whether to assume that the unloaded position is precisely at the origin
  OpenLoop = 0 // global flag for whether data was collected without force clamp for stall force analysis
  MakeButtonPanel()
end
//----------------------------------------------------------------------------------------------------------------------------//
// Routine to construct the button panel
// It's convenient to separate this from above because sometimes we need to re-make the panel without resetting
// the values for the global variables -- e.g., to update from old analyses lacking "interrupted" event termination
macro MakeButtonPanel()
  silent 1; pauseupdate
  NewPanel/W=(1670,50,1910,420) as "New FC Analysis UW" // left,top,right,bottom? coords for chips surface pad
  //NewPanel/W=(2750,50,2990,420) as "New FC Analysis UW" // left,top,right,bottom? coords for chips laptop
  ModifyPanel cbRGB=(65280,43520,0)  // funky orange background color
  Button Loader pos={15,30},size={100,30},proc=LoadButton,title="Load Data", win=Panel0
  CheckBox IsTimePresent pos={125,30},size={100,30},disable=0,title="Time Included?", win=Panel0
  CheckBox IsDataOpenLoop pos={125,45},size={100,30},disable=0,title="Open Loop?", win=Panel0
  Button MarkUnloaded pos={15,65},size={100,30},proc=MarkUnloadedButton,title="Mark Unloaded",win=Panel0
  Button CheckUnloaded pos={125,65},size={100,30},proc=CheckUnloadedButton,title="Check Unloaded",win=Panel0
  Button MarkEvent pos={15,100},size={100,30},proc=EventButton,title="Mark Event", win=Panel0
  Button CheckEvent pos={125,100},size={100,30},proc=CheckButton,title="Check Event",win=Panel0
  Button RemEvnt pos={15,135},size={100,30},proc=RemoveButton,title="Remove Event",win=Panel0
  Button AddEvnt pos={125,135},size={100,30},proc=AddButton,title="Add Event",win=Panel0
  SetVariable setvar0 pos={30,170},size={180,30},title="Current Event",value= EventNumber,win=Panel0
  Button MarkCatast pos={15,195},size={100,30},proc=MarkCatastButton,title="Mark Trans",win=Panel0      // added "transition" analysis but 
  Button RemCatast pos={125,195},size={100,30},proc=RemCatastButton,title="Remove Trans",win=Panel0 // keeping macros named Catast
  Button MarkPauses pos={15,230},size={100,30},proc=MarkPausesButton,title="Mark Pauses",win=Panel0
  Button RemPauses pos={125,230},size={100,30},proc=RemPausesButton,title="Remove Pauses",win=Panel0
  Button MatchCurs pos={70,265}, size={100,30}, proc= MatchCursButton,title="Match Cursors",win=Panel0 
  PopupMenu IncludeADM mode=1, pos={49,300}, size={200,30}, proc=IncludeADMmenu,title="Include ADM",win=Panel0, value="yes;no"
  PopupMenu TermTypeM mode=1, pos={25,335}, size={200,30}, proc=TermTypeMenu,title="Event Termination",win=Panel0, value="other;detach;rescue;interrupted"
  SetDrawEnv textrgb= (0,0,0),fsize=14,fstyle=1  // black text
  DrawText 50,20,"New FC Analysis UW"
end
//----------------------------------------------------------------------------------------------------------------------------//
// All the control functions for the Analysis Panel are below.
// Since buttons have to call functions, but all the code is in macros, these just call the macros!
Function LoadButton(ctrlName) : ButtonControl
  String ctrlName
  ControlInfo IsTimePresent
  switch(V_Value)
    case 1:
    Execute "LoadDam1DataJD()"
      break
    default:
    Execute "LoadDam1Data()"
  endswitch
Execute "StartForceClampAnalysis()"
End
Function MarkUnloadedButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "MarkUnloaded()"
End
Function CheckUnloadedButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "CheckUnloaded()"
End
Function EventButton(ctrlName) : ButtonControl
  String ctrlName
  ControlInfo IsDataOpenLoop // this bit checks the status of the "Open Loop?" checkbox and sets global variable OpenLoop
  NVAR OpenLoop
  OpenLoop = V_value
  Execute "MarkEvent()"
End
Function CheckButton(ctrlName) : ButtonControl
  String ctrlName
  ControlInfo IsDataOpenLoop // this bit checks the status of the "Open Loop?" checkbox and sets global variable OpenLoop
  NVAR OpenLoop
  OpenLoop = V_value
  Execute "CheckEvent()"
End
Function RemoveButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "RemoveEvent()"
End
Function AddButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "AddEvent()"
End
Function CheckCurrentButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "CheckCurrent()"
End
Function MarkCatastButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "MarkCatast()"   // keeping macros named Catast
End
Function RemCatastButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "RemoveCatast()"  // keeping macros named Catast  
End
Function MarkPausesButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "MarkPauses()"
End
Function RemPausesButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "RemovePauses()"
End
Function MatchCursButton(ctrlName) : ButtonControl
  String ctrlName
  Execute "MatchCursors()" 
End
Function IncludeADMmenu(ctrlName,popNum,popStr) : PopupMenuControl
  String ctrlName
  Variable popNum
  String popStr
  String dummystring = "SetIncludeADM("+num2str(popNum)+")"
  Execute dummystring
End
Function TermTypeMenu(ctrlName,popNum,popStr) : PopupMenuControl
  String ctrlName
  Variable popNum
  String popStr
  String dummystring = "SetTermType("+num2str(popNum)+")"
  Execute dummystring
End
//----------------------------------------------------------------------------------------------------------------------------//
macro StartForceClampAnalysis()
  silent 1; pauseupdate
  variable i=0
  Make/O/N=1000 StartTime,PauseTime,CatTime,EndTime  // waves for beginning, end and catastrophe times for each event
  Make/O/N=1000 F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term // waves for other info for each event
  Make/O/N=1000 npexit,tpt,npentr,trt,v1,v2 // waves for pause statistics and characteristic velocities for each event
  do // initialize
    StartTime[i] = NaN
    PauseTime[i] = NaN
    CatTime[i] = NaN 
    EndTime[i] = NaN
    F[i] = NaN // total force
    Fx[i] = NaN // x comp of force
    Fy[i] = NaN // y comp of force
    eF[i] = NaN // error on total force
    eFx[i] = NaN // error on x comp
    eFy[i] = NaN // error on y comp
    thetaF[i] = NaN // angle of force
    thetaS[i] = NaN // angle of stage movement
    Va[i] = NaN // avg speed of ADM
    Ta[i] = NaN // total time of ADM
    Da[i] = NaN // total displacement during ADM
    Vp[i] = NaN // avg speed during pause
    Tp[i] = NaN // total time during pause
    Dp[i] = NaN // total displacement during pause
    Vd[i] = NaN // avg speed of DDM
    Td[i] = NaN // total time of DDM
    Dd[i] = NaN // total displacement during DDM
    term[i] = NaN // termination type: other = NaN, detach = 0, rescue = 1, interrupted = 2  
    npexit[i] = NaN // number of pause exits
    tpt[i] = NaN // total pause time
    npentr[i] = NaN // number of pause entries
    trt[i] = NaN // total run time
    v1[i] = NaN // characteristic velocity during pauses
    v2[i] = NaN // characteristic velocity during runs
    i+=1
  while (i<1000)
  Make/O/N=2 vline,vlinet // wave for displaying vertical lines on graphs
  vline[0]=-10000; vline[1]=10000; vlinet[0]=0; vlinet[1]=0
 
  // Create smoothed versions of the bead position data
  SmoothBeadPosWaves(7)  // user can redo this smoothing with different number of smpoints

  // Display stage movements on upper graph
  Display sX,sY
  ModifyGraph fSize=8, btLen=4
  Label left "Stage position (nm)"
  Label bottom "Time (s)"
  ModifyGraph rgb(sX)=(65280,0,0),rgb(sY)=(0,0,52224)
  ShowInfo
  DoWindow /C $"StagePos" // renames this graph "StagePos"
  ModifyGraph standoff=0
  SetWindow StagePos, hook=QuickCursors, hookcursor=0, hookEvents=1

  // Display bead-trap separation on lower graph
  Display bY,bYs,bX,bXs
  ModifyGraph fSize=8, btLen=4
  Label left "Bead-trap separation (nm)"
  ModifyGraph rgb(bX)=(65280,49152,48896),rgb(bY)=(48896,49152,65280)
  ModifyGraph rgb(bXs)=(65280,0,0),rgb(bYs)=(0,0,65280)
  ShowInfo
  DoWindow /C $"BTSep" // renames this graph "BTSep"
  SetAxis left -130,130
  ModifyGraph standoff=0
  SetWindow BTSep, hook=QuickCursors, hookcursor=0, hookEvents=1
  //ModifyGraph grid(left)=1,manTick(left)={0,32,0,0},manMinor(left)={3,0} // adds the 8-nm gridlines
  ModifyGraph margin(top)=36 // extra margin for bigger button
  Button MarkEvent2 pos={0,0},size={700,30},proc=EventButton,title="Mark Event" // big button
  Label bottom "Total Events Selected = "+num2str(TotalEvents)
  
  PositionAllWindows() // Moves the graphs into convenient positions for simultaneous viewing
end
//----------------------------------------------------------------------------------------------------------------------------//
macro MarkUnloaded()
  silent 1; pauseupdate
  variable big,small
  DoWindow/F 'BTSep'
  if ( (stringmatch(CsrInfo(A),"")) && (stringmatch(CsrInfo(B),"")) ) // if the cursors are not on the graph
    Print "No cursors on graph!"
  else
    big = max(xcsr(A),xcsr(B)) // re-store start and end of unloaded data
    small = min(xcsr(A),xcsr(B))
    StartTime[0] = small
    EndTime[0] = big
    UnloadedIsMarked = 1 // set flag so we know unloaded segment is marked
    Cursor /K A // and remove cursors from the graph
    Cursor /K B
    DoWindow/F 'StagePos'
    Cursor /K A
    Cursor /K B
    DoWindow/F 'BTSep'
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro CheckUnloaded()
  silent 1; pauseupdate
  if (UnloadedIsMarked == 1)
    DoWindow/F 'BTSep'
    Cursor  A,bX,StartTime[0]
    Cursor  B,bX,EndTime[0]
    MatchCursors()  // updates cursors on StagePos and moves cursors to appropriate waves 
    DoWindow/F 'BTSep'
  else
    print "Unloaded data has not been marked yet"
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro MarkEvent()
  silent 1; pauseupdate
  variable big,small
  if (EventNumber>=1)
    if (UnloadedIsMarked == 1)
      DoWindow/F 'BTSep'
      if ( (stringmatch(CsrInfo(A),"")) || (stringmatch(CsrInfo(B),"")) ) // if both cursors are not on the graph
        Print "Both cursors are not on the graph"
      else
        big = max(xcsr(A),xcsr(B)) // re-store start and end of unloaded data
        small = min(xcsr(A),xcsr(B))
        StartTime[EventNumber] = small
        EndTime[EventNumber] = big
        if (EventNumber <= TotalEvents)  // if we're looking at an event we've already selected
          EventNumber = EventNumber + 1  // update current event number...
          if (EventNumber <= TotalEvents)
            CheckEvent() // and mark the new event if it exists
          else
            RemoveCursors() // or just remove cursors if next event hasn't been marked yet
          endif
        else  // if we're selecting a new event
          EventNumber = EventNumber + 1  // update current event number
          TotalEvents = TotalEvents + 1       // and update the total event counter
          Label bottom "Total Event Selected = "+num2str(TotalEvents)
          RemoveCursors()
        endif
      endif
    else
      Print "Cannot mark events until unloaded is marked"
    endif
    else
      Print "Event 0 is reserved for unloaded"
      EventNumber = 1 // force user to look at event 1
      CheckEvent()
    endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro CheckEvent()
  silent 1; pauseupdate
  if ((EventNumber <= TotalEvents) && (EventNumber >= 1)) // if you're not trying to check an event that doesn't exist
    DoWindow/F 'BTSep'
    Label bottom "Total Event Selected = "+num2str(TotalEvents)
    Cursor  A,bX,StartTime[EventNumber]
    Cursor  B,bX,EndTime[EventNumber]
    MatchCursors()  // updates cursors on StagePos and moves cursors to appropriate waves
    UpdateIncludeADMmenu() // updates value displayed on "include ADM" pull-down menu
    UpdateTermTypeMenu() // updates value displayed on "event termination" pull-down menu
    if (StartedFitting == 0) // if this is the first event that's been checked
      FitAll() // then fit all the events
      FitEvent(EventNumber) // refit current event so it's the one that gets displayed
      UpdateOverview() // update the overview graph if it exists
    else
      FitEvent(EventNumber) // otherwise just re-fit the current event
      UpdateOverview() // update the overview graph if it exists
    endif
  else
    print "Event number "+num2str(EventNumber)+" does not exist"
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro RemoveEvent()
  silent 1; pauseupdate
  if (EventNumber >= 1)
    if (UnloadedIsMarked == 1)
      if (EventNumber <= TotalEvents)  // if we're looking at an event we've already selected
        deletepoints EventNumber,1,StartTime,PauseTime,CatTime,EndTime,F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,npexit,tpt,npentr,trt,v1,v2
        RemoveCursors()
        TotalEvents = TotalEvents - 1
        dowindow/F BTSep
        label bottom "Total Event Selected = "+num2str(TotalEvents)
        CheckEvent()
      else
        print "No event number "+num2str(EventNumber)+" yet!"
      endif
    else
      print "Cannot remove events until unloaded is marked"
    endif
  else
    print "Event 0 is reserved for unloaded"
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro AddEvent()
  silent 1; pauseupdate
  if (EventNumber >= 1)
    if (UnloadedIsMarked == 1)
      if (EventNumber <= TotalEvents)  // if we're looking at an event we've already selected
        insertpoints EventNumber,1,StartTime,PauseTime,CatTime,EndTime,F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,npexit,tpt,npentr,trt,v1,v2
        PauseTime[EventNumber] = NaN
        Vp[EventNumber] = NaN; Tp[EventNumber] = NaN; Dp[EventNumber] = NaN // might as well init these too for neatness
        CatTime[EventNumber] = NaN // need to be sure this is NaN since its used later as a flag for catastrophes
        Vd[EventNumber] = NaN; Td[EventNumber] = NaN; Dd[EventNumber] = NaN // might as well init these too for neatness
        term[EventNumber] = NaN
        npexit[EventNumber] = NaN; tpt[EventNumber] = NaN; npentr[EventNumber] = NaN; trt[EventNumber] = NaN; v1[EventNumber] = NaN; v2[EventNumber] =  NaN
        TotalEvents+=1
        dowindow/F BTSep
        label bottom "Total Event Selected = "+num2str(TotalEvents)
        MarkEvent() // note that this increases EventNumber by one
        EventNumber -= 1 // therefore I need to decrease by one here to Check the newly added event
        CheckEvent()
      else
         print "No event number "+num2str(EventNumber)+" yet!"
      endif
    else
      print "Cannot add events until unloaded is marked"
    endif
  else
    print "Event 0 is reserved for unloaded"
  endif
end
//----------------------------------------------------------------------------------------------------------------------------------------------------------//
macro FitAll()
  silent 1; pauseupdate
  variable k=1
  do
    FitEvent(k)
    k+=1
  while (k<=TotalEvents)
  StartFitting() // creates the new graphs, after running through all the fits to avoid flashing display
  StartedFitting = 1 // sets flag so other routines can check if the graphs are created yet
end
//----------------------------------------------------------------------------------------------------------------------------------------------------------//
macro FitEvent(i)
  variable i
  silent 1; pauseupdate
  variable startt,pauset,catt,endt,dumnum
  
  startt = StartTime[i]
  pauset = PauseTime[i]
  catt = CatTime[i]
  endt = EndTime[i]
  
  // create cropped stage pos waves for event i, remove offsets
  duplicate /o/r=(startt,endt) sX,sXi // stage x pos
  duplicate /o/r=(startt,endt) sY,sYi // stage y pos
  duplicate /o/r=(startt,endt) sY,sR  // stage radial distance
  dumnum=sXi[0]
  sXi-=dumnum // subtracts initial x pos, so stage pos is relative to start of event
  dumnum=sYi[0]
  sYi-=dumnum // subtracts initial y pos

  // crop timestamp wave for JD
  //duplicate /o/r=(startt,endt) timestamp, Ti // timestamp

  // create cropped bead-trap sep and force waves for event i
  duplicate /o/r=(StartTime[0],EndTime[0]) bX,bX0 // bead x pos for event 0, assumed to be unloaded
  duplicate /o/r=(StartTime[0],EndTime[0]) bY,bY0 // bead y pos for event 0
  duplicate /o/r=(startt,endt) bX,bXi // bead x pos for current event
  duplicate /o/r=(startt,endt) bY,bYi // bead y pos for current event
  duplicate /o/r=(startt,endt) bY,Fi // total force for current event
  duplicate /o/r=(startt,endt) bY,sFi // total force for current event
  duplicate /o bXi,sepX // temporary wave for  bead-trap sep or force along x
  duplicate /o bYi,sepY // temporary wave for bead-trap sep or force along y
  if (IgnoreUnloaded != 1)
    wavestats /Q bX0
    sepX-=V_avg // correction for error in rest position
    wavestats /Q bY0
    sepY-=V_avg // correction for error in rest position
  endif
  sepX*=kx[0] // conversion to force using stored trap stiffness in x
  sepY*=ky[0] // conversion to force using stored trap stiffness in y
    
 // store mean force values
 wavestats/q sepX
 Fx[i] = V_avg
 eFx[i] = V_sdev // note force uncertainty assumes error arises solely from thermal motion (no contribution from stiffness estimate)
 wavestats/q sepY
 Fy[i] = V_avg
 eFy[i] = V_sdev
 F[i] = sqrt(Fx[i]^2+Fy[i]^2)
 eF[i] = sqrt((Fx[i]*eFx[i]/F[i])^2+(Fy[i]*eFy[i]/F[i])^2) // error in total F using propagation of errors formula
 thetaF[i] = atan2(Fy[i],Fx[i])*180/PI
 
 // compute angle of stage movement
 CurveFit/Q line sYi /X=sXi // note that this fit can puke if the stage move is perfectly vertical
 if (numtype(W_coef[1]) == 2) // if curvefit pukes
   thetaS[i] = 90 // then assume stage moves are perfectly vertical
 else
   thetaS[i] = atan(W_coef[1])*180/PI // otherwise compute arctan of slope (note that direction is ambiguous here)
 endif
 If (abs(thetaS[i]+180-thetaF[i]) < abs(thetaS[i]-thetaF[i])) // pick direction closest to direction of force
   thetaS[i]+=180
 else
   If (abs(thetaS[i]-180-thetaF[i]) < abs(thetaS[i]-thetaF[i]))
     thetaS[i]-=180
   endif
 endif
   
  // compute projections along the direction of clamping
  if (OpenLoop == 0) // normally this program assumes force clamping
    sR = rotX( sXi, sYi, (-1*thetaS[i]) )
    Fi = rotX( sepX, sepY, (-1*thetaS[i]) )
  else // if data is recorded open loop, then use the bead-trap sep rather than stage motion to define angle
    sR = rotX( sXi, sYi, (-1*thetaF[i]) )
    Fi = rotX( sepX, sepY, (-1*thetaF[i]) )
  endif
  duplicate/o Fi, sFi
  smooth/b=1 100,sFi
  Killwaves sepX,sepY
    
  //compute vels, durations and distances
  if (StartedFitting == 1) // if the fitting windows have been created
    DoWindow/F VelHisto // bring velocity histogram window to front
    RemoveFromGraph/Z fit_sRvel_hist
    DoWindow/F Velocities // bring velocity trace window to front
    RemoveFromGraph/Z pauseflag,velatentries,velatexits
    DoWindow /F EventFit // bring position trace window to front
    RemoveFromGraph/Z fit_sRd
    RemoveFromGraph/Z fit_sRp
  endif
  if (numtype(catt) == 2) // if catt is an NaN; both catt and pauset will be NaN if no transition is marked
    dumnum=endt // then fit up to the end of the event
  else
    dumnum=pauset // if catt is a real number then fit ADM up until pause
    Killwaves/Z fit_sRd // must be sure this wave doesn't exist before subsequent rename
    Killwaves/Z fit_sRp // must be sure this wave doesn't exist before subsequent rename
    
    // Pause Fitting
    CurveFit/Q line  sR (pauset,catt) /D // line fits to get velocity during pause
    ModifyGraph rgb(fit_sR)=(0,65280,0)
    Rename fit_sR fit_sRp
    Vp[i] = W_coef[1]
    Tp[i] = catt - pauset
    Dp[i] = sR[x2pnt(sR,catt)] - sR[x2pnt(sR,pauset)]
    if (OpenLoop == 1) // for open loop data the "pause" is instead used to represent the stall interval, over which the force should be re-computed
       Fx[i] = NaN
       eFx[i] = NaN
       Fy[i] = NaN
       eFy[i] = NaN
       Vp[i] = NaN
       Dp[i] = NaN
       wavestats/q/r=(pauset,catt) Fi
       F[i] = V_avg
       eF[i] = V_sem
    endif	
    
    // DDM Fitting
    variable midd = 0.5*(sR[x2pnt(sR,catt)]-sR[x2pnt(sR,endt)])+sR[x2pnt(sR,endt)]  // midpoint (or whatever) % of DDM distance defined from end position
    findlevel /Q/R=(catt,endt) sR, midd
    CurveFit/Q line  sR (V_LevelX,endt) /D // line fits to get velocity during DDM
    ModifyGraph rgb(fit_sR)=(0,0,65280)
    Rename fit_sR fit_sRd
    Vd[i] = W_coef[1]
    Td[i] = endt - catt
    Dd[i] = sR[x2pnt(sR,endt)] - sR[x2pnt(sR,catt)]
    										
  endif
  
  // ADM Fitting
  ControlInfo /W=Panel0 IncludeADM
  if(V_value == 1) // this ADM is to be included
    CurveFit/Q line  sR (startt,dumnum) /D // line fits to get velocities
    Va[i] = W_coef[1]
    Ta[i] = dumnum - startt
    Da[i] = sR[x2pnt(sR,dumnum)] - sR[x2pnt(sR,startt)]
  else // this ADM is to be ignored
    Va[i] = NaN
    Ta[i] = NaN
    Da[i] = NaN
    DoWindow/F EventFit
    RemoveFromGraph/Z fit_sR
  endif	
  
  // analysis of instantaneous velocities during the ADM
  if (numtype(catt) == 2) // catt will be NaN if no transition is marked
    dumnum=endt // fit up to the end of the event
  else
    dumnum=catt // catt is a real number, so fit up until catastrophe
  endif
  duplicate/o/r=(startt,dumnum) sR, sRvel
  ControlInfo /W=Panel0 IncludeADM
  if(V_value == 1) // this ADM is to be included
    smooth /b=1 1001,sRvel // might want to adjust this smoothing
  endif
  differentiate sRvel
  make/N=500/O sRvel_hist
  histogram/B=1 sRvel,sRvel_hist
  duplicate/o sRvel,smsRvel // for additional smoothing after differentiation
  ControlInfo /W=Panel0 IncludeADM
  if(V_value == 1) // this ADM is to be included
    smooth /b=1 1001,smsRvel // might want to adjust this smoothing
    //ClassifyPauses()    
  endif
  						
end
//---------------------------------------------------------------------------------------------------------------------------//
// create graph with close up of the event
macro StartFitting()
  silent 1; pauseupdate
  Display/R Fi
  AppendToGraph/R sFi
  AppendToGraph sR
  ModifyGraph rgb(Fi)=(47872,47872,47872)
  ModifyGraph mode(Fi)=2
  ModifyGraph rgb(sR)=(0,0,0)
  ModifyGraph standoff=0
  ModifyGraph fSize=8, btLen=4
  Label left "Stage position (nm)"
  Label bottom "Time (s)"
  Label right "Force (pN)" 
  DoWindow /C $"EventFit" // renames this graph "EventFit"
  SetWindow EventFit, hook=QuickCursors, hookcursor=0, hookEvents=1
  Cursor /H=1 A sR pnt2x(sR,0); Cursor /K A // sets cursor A to be a crosshair
  Cursor /H=1 B sR pnt2x(sR,0); Cursor /K B // sets cursor B to be a crosshair 
  
  // create graph with velocity vs time
  Display sRvel, smsRvel
  Label left "Velocity (nm/s)"
  Label bottom "Time (s)"
  ModifyGraph standoff=0
  ModifyGraph fSize=8, btLen=4
  ModifyGraph rgb(sRvel)=(52224,52224,52224),rgb(smsRvel)=(0,0,0)
  dowindow /c $"Velocities"

  // create histogram of instantaneous velocities
  Display sRvel_hist
  Label left "Counts"
  Label bottom "Velocity (nm/s)"
    ModifyGraph standoff=0
  ModifyGraph fSize=8, btLen=4
  ModifyGraph mode=6
  ModifyGraph rgb=(34816,34816,34816)
  dowindow /c $"VelHisto"
  SetWindow VelHisto, hook=QuickCursors, hookcursor=0, hookEvents=1
  Cursor /H=2 A sRvel_hist pnt2x(sRvel_hist,0); Cursor /K A // sets cursor A to be vertical crosshair
  Cursor /H=2 B sRvel_hist pnt2x(sRvel_hist,5); Cursor /K B // sets cursor B to be vertical crosshair 

  // create an XY plot
  Display bY0 vs bX0
  AppendToGraph bYi vs bXi
  SetAxis left -150,150
  SetAxis bottom -150,150
  AppendToGraph sYi vs sXi
  ModifyGraph mode=2,rgb(bY0)=(52224,52224,52224),rgb(bYi)=(34816,34816,34816),rgb(sYi)=(0,0,0)
  ModifyGraph grid=1
  Label left "\\Z08Y position (nm)"
  Label bottom "X position (nm)"
  ModifyGraph standoff=0
  ModifyGraph fSize=8, btLen=4
  DoWindow /C $"XYplot" // renames this graph "XYplot"
  PositionAllWindows() // Moves the graphs into convenient positions for simultaneous viewing
end
//----------------------------------------------------------------------------------------------------------------------------//
// modified to include pause events but keeping the name
macro MarkCatast()
  silent 1; pauseupdate
  variable startt=0,endt=0
  if (EventNumber>=1)
    if (StartedFitting == 1)
      DoWindow/F 'EventFit'
      if (stringmatch(CsrInfo(A),"")) // if cursor A is not on the graph
        if (stringmatch(CsrInfo(B),"")) // if cursor B is also not on the graph
          Print "No cursors are on the graph"
        else // cursor B is on graph
          Print "Only one cursor is on the graph"
        endif
      else // cursor A is on the graph
        if (stringmatch(CsrInfo(B),"")) // cursor A is on graph but B is not
          Print "Only one cursor is on the graph"
        else // both cursors are on the graph
          startt = min(xcsr(A),xcsr(B))
          endt = max(xcsr(A),xcsr(B))
          Cursor /K A
          Cursor /K B
        endif
      endif
      if (startt!=0) // only mark transitions if both cursors are on the graph
        PauseTime[EventNumber] = startt
        CatTime[EventNumber] = endt
        FitEvent(EventNumber) // update the fit given the new transition times
      endif
    else
      Print "Cannot mark transitions until fitting has started"
    endif
  else
    Print "Event 0 is reserved for unloaded"
    EventNumber = 1 // force user to look at event 1
    CheckEvent()
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro RemoveCatast()
  silent 1; pauseupdate
  PauseTime[EventNumber] = NaN
  CatTime[EventNumber] = NaN
  Vd[EventNumber] = NaN
  Td[EventNumber] = NaN
  Dd[EventNumber] =  NaN
  Vp[EventNumber] = NaN
  Tp[EventNumber] = NaN
  Dp[EventNumber] =  NaN
  RemoveFromGraph/Z fit_sRd
  KillWaves/Z fit_sRd
  RemoveFromGraph/Z fit_sRp
  KillWaves/Z fit_sRp
  FitEvent(EventNumber)
end
//----------------------------------------------------------------------------------------------------------------------------//
// removes cursors from several graphs
macro RemoveCursors()
  silent 1; pauseupdate
  DoWindow/F 'BTSep'
  Cursor /K A
  Cursor /K B
  DoWindow/F 'StagePos'
  Cursor /K A
  Cursor /K B
  DoWindow/F 'BTSep'
end
//----------------------------------------------------------------------------------------------------------------------------//
// Smooth the bead position waves
macro SmoothBeadPosWaves(smpoints)
  variable smpoints
  silent 1; pauseupdate
  Duplicate /O bX bXs
  Duplicate /O bY bYs
  //MedianSmooth(bXs,smpoints) // old method using the MedianSmooth XOP that Josh dug up
  //MedianSmooth(bYs,smpoints)
  Smooth /M=0 smpoints, bXs // new method using Smooth function that now includes median filtering
  Smooth /M=0 smpoints, bYs
end
//----------------------------------------------------------------------------------------------------------------------------//
macro FinalVals()
  silent 1; pauseupdate
  Edit StartTime,EndTime,F,eF,Fx,eFx,Fy,eFy,thetaF,thetaS,Va,Ta,Da,PauseTime,Vp,Tp,Dp,CatTime,Vd,Td,Dd,term,npexit,tpt,npentr,trt,v1,v2
  ModifyTable size=8,width=45
end
//----------------------------------------------------------------------------------------------------------------------------//
macro MatchCursors()
  silent 1; pauseupdate
  variable startt,endt,meanbX,meanbY
  startt = min(xcsr(A),xcsr(B))
  endt = max(xcsr(A),xcsr(B))
  DoWindow EventFit
  if (V_flag != 0) // window EventFit exists
    DoWindow /F EventFit
    Cursor  /K A
    Cursor  /K B
  endif
  // Place cursors on relevant traces within BTSep and StagePos plots
  WaveStats /Q/R=(startt,endt) bX
  meanbX = V_avg
  WaveStats /Q/R=(startt,endt) bY
  meanbY = V_avg
  if (meanbX >= meanbY)
    DoWindow/F 'BTSep'
    Cursor  A,bX,startt
    Cursor  B,bX,endt
    DoWindow/F 'StagePos'
    Cursor  A,sX,startt
    Cursor  B,sX,endt
 else
    DoWindow/F 'BTSep'
    Cursor  A,bY,startt
    Cursor  B,bY,endt
    DoWindow/F 'StagePos'
    Cursor  A,sY,startt
    Cursor  B,sY,endt
  endif  
end
//----------------------------------------------------------------------------------------------------------------------------//
macro SetTermType(popNum)
  variable popNum
  silent 1; pauseupdate
  if (popNum == 1) // user specified "other"
    term[EventNumber] = NaN
  else
    if (popNum == 2) // user specified "detach"
      term[EventNumber] = 0
    else // user specified "rescue"
      if (popNum ==3) // user specified "rescue"
        if (numtype(CatTime[EventNumber]) == 2) // but event has no DDM
          term[EventNumber] = NaN
          UpdateTermTypeMenu() // forces menu back to "other"
          Print "Event does not include DDM"
        else // event has DDM
          term[EventNumber] = 1
        endif
      else // user specified "interrupted"
        term[EventNumber] = 2
      endif
    endif
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro UpdateTermTypeMenu()
  silent 1; pauseupdate
  variable dummy
  if (numtype(term[EventNumber]) == 2) // if term value for current event is an NaN, specifying "other"
    dummy = 1
  else
    dummy = term[EventNumber]+2
  endif
  PopupMenu TermTypeM mode=(dummy),win=Panel0 // updates value displayed in pull down menu
end
//----------------------------------------------------------------------------------------------------------------------------//
macro SetIncludeADM(popNum)
  variable popNum
  silent 1; pauseupdate
  if (popNum == 1) // user specified "yes"
    if (numtype(Va[EventNumber]) == 2) // but ADM stats were previously erased
      FitEvent(EventNumber) // refit to re-record ADM stats
    else // ADM stats are still recorded
      // no need to do anything
    endif
  else // user specified "no"
    Va[EventNumber] = NaN  // erases ADM stats
    Ta[EventNumber] = NaN
    Da[EventNumber] = NaN
    FitEvent(EventNumber) // refit
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro UpdateIncludeADMmenu()
  silent 1; pauseupdate
  variable dummy
  if (numtype(Va[EventNumber]) == 2) // if ADM stats were erased for current event
    dummy = 2
  else
    dummy = 1
  endif
  PopupMenu IncludeADM mode=(dummy),win=Panel0 // updates value displayed in pull down menu
end
//----------------------------------------------------------------------------------------------------------------------------//
// simple macro to spit rupture information to command line
macro rup()
  silent 1; pauseupdate
  print filename[0]+"	"+num2str(kx[0])+"	"+num2str(ky[0])+"	"+num2str(vcsr(A))
  //print initialcomments[2]+"	"+num2str(vcsr(A))
end
//----------------------------------------------------------------------------------------------------------------------------//
// simple macro to analyze tubulin wave data
macro wav()
  silent 1; pauseupdate
  variable avg_force, sdev_force, avg_baseline, sdev_baseline, start_point, peak, peak_time, wheight, risetime
  wavestats/q/r=[pcsr(A,"EventFit"),pcsr(B,"EventFit")] sFi
  avg_force = V_avg
  sdev_force = V_sdev
  wavestats/q/r=[pcsr(A,"EventFit"),pcsr(B,"EventFit")] sR
  avg_baseline = V_avg
  sdev_baseline = V_sdev
  start_point = max(pcsr(A,"EventFit"),pcsr(B,"EventFit"))
  wavestats/q/r=[start_point,numpnts(sR)] sR
  peak = V_min
  peak_time = V_minloc
  wheight = avg_baseline - peak
  risetime = peak_time - pnt2x(sR,start_point)
  
  //print filename[0]+"	"+num2str(avg_force)+"	"+num2str(sdev_force)+"	"+num2str(wheight)+"	"+num2str(risetime)+"	000	"+num2str(kx[0])+"	tub	oper	note" // prints to command line for pasting to excel
  make/o/n=1 mf,emf,amp,rt,ang,goodamp,goodrt // store vals in waves for export to other igor experiments
  mf[0]=avg_force
  emf[0]=sdev_force
  amp[0]=wheight
  rt[0]=risetime
  ang[0]=thetaF[1]
  goodamp[0]=nan
  goodrt[0]=nan
end
//----------------------------------------------------------------------------------------------------------------------------//
// special panel for user input regarding tubulin wave data
function tubulinwavedialogpanel()
  silent 1; pauseupdate
  newpanel/k=2/w=(600,300,800,415) // left,top,right,bottom
  dowindow/c tubulinwavepanel // renames the window "tubulinwavepanel"
  setdrawenv xcoord=rel, ycoord=rel, textxjust=1, textyjust=1 // use relative coords and center justify text
  drawtext 0.5,0.2,"Which values are trustworthy?"
  button donebutton pos={20,40},size={60,60},proc=donebuttonproc,title="done"
  checkbox ampcheckbox pos={90,50},size={60,20},value=1,disable=0,title="include amp"
  checkbox risetimecheckbox pos={90,75},size={60,20},value=1,disable=0,title="include risetime"
  pauseforuser tubulinwavepanel, EventFit // pauses operation while user adjusts EventFit graph window
  return 0
end
function donebuttonproc(ctrlName) : buttoncontrol
  string ctrlName
  Execute "wav()" // recalculates the force, forceerror, pulse amplitude, pulse risetime, and force angles
  wave goodamp
  wave goodrt
  controlinfo ampcheckbox // value of checkbox placed into V_value
  if(V_value!=1)
    goodamp[0]=0 // if user doesn't like amp, set goodamp to zero
  else
    goodamp[0]=1 // if user likes amp, set goodamp to one
  endif
  controlinfo risetimecheckbox // value of checkbox placed into V_valu
  if(V_value!=1)
    goodrt[0]=0 // if user doesn't like risetime, set goodrt to zero
  else
    goodrt[0]=1 // if user likes risetime, set goodrt to one
  endif
  dowindow/k tubulinwavepanel // kill the panel, release from pause
  //edit mf,emf,amp,rt,ang,goodamp,goodrt
end
//----------------------------------------------------------------------------------------------------------------------------//
// macro for semiautomatic analysis of pausing during assembly phases, styled after Keir Neuman's method!
macro ClassifyPauses()
  silent 1; pauseupdate
  variable minduration=7 // minimum allowed duration (in seconds) for run and pause intervals
  variable amp1,vel1,amp2,vel2 // fit parameters for sum of two gaussians
  variable vthresh // threshold velocity for distinguishing pauses from runs
  variable dumnum,i=0 // dummy variables, to hold last time in velocity trace, and for iteration counting
  variable tfirstexit,tfirstentry // times of the first pause exit and first pause entry, for determining initial state
  if (EventNumber>=1)
    if (StartedFitting == 1)
      DoWindow/F 'VelHisto'
      if (numtype(v1[EventNumber])==0) // characteristic velocities have already been stored
        vel1 = v1[EventNumber]  // use previously stored characteristic vel as initial guess for gaussian fit
        amp1 = sRvel_hist[x2pnt(sRvel_hist,vel1)]
        vel2 = v2[EventNumber]  // use previously stored characteristic vel as initial guesses for gaussian fit
        amp2 = sRvel_hist[x2pnt(sRvel_hist,vel2)]
        make/d/n=6/o w_coef
        w_coef = {amp1,vel1,1,amp2,vel2,1}
        funcFit/q/n/h="010000"/nthr=0 SumTwoGaus w_coef  sRvel_hist /d // guassian fitting, 1st peak pos held fixed
        //funcFit/q/n/nthr=0 SumTwoGaus w_coef  sRvel_hist /d // guassian fitting, both peaks allowed to float
        v1[EventNumber]=w_coef[1] // store characteristic velocity during pauses
        v2[EventNumber]=w_coef[4] // store characteristic velocity during runs
        vthresh = w_coef[1] + (w_coef[4]-w_coef[1])/2 // threshold chosen halfway between the gaussian peaks
        findlevels/q/edge=0 smsRvel, vthresh // find times of pause exits and entries
        make/o/n=(numpnts(w_findlevels)+2) alltimes,pauseflag // waves to add shading of pauses on vel trace
        alltimes=w_findlevels[p-1] // alltimes will include all the pause entry and exit times
        alltimes[0]=StartTime[EventNumber] // alltimes also includes the start of the trace
        pauseflag=0 // initialize to zeros
        if (numtype(CatTime[EventNumber]) == 2) // catt will be NaN if no transition is marked
          dumnum=EndTime[EventNumber] // fit up to the end of the event
        else
          dumnum=CatTime[EventNumber] // catt is a real number, so fit up until catastrophe
        endif
        alltimes[numpnts(alltimes)-1]=dumnum // alltimes also includes the end of the velocity trace        
        make/o/n=(numpnts(alltimes)-1) intervals,velavgs // waves for time intervals and corresponding average vels
        intervals=alltimes[p+1]-alltimes[p]  // fill intervals wave from alltimes
        do // sequentially search for and remove intervals that are too small
          if (intervals[i]<minduration) // interval is less than minimum duration
            if (i==0)
              alltimes[i+1]=NaN
            else
              if (i==(numpnts(intervals)-1))
                alltimes[i]=NaN
              else
                alltimes[i]=NaN
                alltimes[i+1]=NaN
              endif
            endif
            extract/o alltimes,alltimes,(numtype(alltimes)==0) // strip NaNs
            make/o/n=(numpnts(alltimes)-1) intervals,velavgs // remake intervals and velavg waves
            intervals=alltimes[p+1]-alltimes[p] // refill intervals wave from alltimes
            i=-1 // this forces the search to start again if any too-small intervals have been found and removed  
          else // interval is greater than or equal to minimum duration
            velavgs[i]=mean(smsRvel,alltimes[i],alltimes[i+1])  // compute average velocity for the interval
          endif
          i+=1
        while(i<numpnts(intervals))
        if(numpnts(intervals)<2) // there is only one interval, so no crossings were found
          npexit[EventNumber]=0
          tpt[EventNumber]=0
          npentr[EventNumber]=0
          trt[EventNumber]=sum(intervals) // the one interval is assumed here to be a run
        else // there is more than one interval, so at least one crossing was found
          duplicate/o alltimes,crossingtimes
          deletepoints (numpnts(crossingtimes)-1),1,crossingtimes
          deletepoints 0,1,crossingtimes
          // separate the pause exits from the pause entries
          extract/o velavgs,evenvelavgs,(mod(p,2)==0) // extract velocity avgs for even-numbered intervals
          if(mean(evenvelavgs) < vthresh) // trace began in pause state
            extract/o intervals,pausedurations,(mod(p,2)==0)
            extract/o intervals,rundurations,(mod(p,2)!=0)
            extract/o crossingtimes,exittimes,(mod(p,2)==0)
            extract/o crossingtimes,entrytimes,(mod(p,2)!=0)
            pauseflag=mod(p+1,2)
          else // trace began in run state
            extract/o intervals,pausedurations,(mod(p,2)!=0)
            extract/o intervals,rundurations,(mod(p,2)==0)
            extract/o crossingtimes,entrytimes,(mod(p,2)==0)
            extract/o crossingtimes,exittimes,(mod(p,2)!=0)
            pauseflag=mod(p,2)          
          endif
          duplicate/o exittimes,velatexits
          if(numpnts(velatexits)>0)
            velatexits=smsRvel[x2pnt(smsRvel,exittimes)]
          endif
          duplicate/o entrytimes,velatentries
          if(numpnts(velatentries)>0)
            velatentries=smsRvel[x2pnt(smsRvel,entrytimes)]
          endif
          npexit[EventNumber]=numpnts(exittimes)
          tpt[EventNumber]=sum(pausedurations)
          npentr[EventNumber]=numpnts(entrytimes)
          trt[EventNumber]=sum(rundurations)
          DoWindow/F 'Velocities'
          appendtograph velatentries vs entrytimes // display pause intervals on velocity trace
          ModifyGraph mode(velatentries)=3,marker(velatentries)=8
          ModifyGraph rgb(velatentries)=(65280,0,0)
          appendtograph velatexits vs exittimes
          ModifyGraph mode(velatexits)=3,marker(velatexits)=8,rgb(velatexits)=(0,52224,0)
          appendtograph/r pauseflag vs alltimes
          ModifyGraph mode(pauseflag)=5,hbFill(pauseflag)=2,toMode(pauseflag)=1
          ModifyGraph rgb(pauseflag)=(60928,60928,60928)
          ReorderTraces sRvel,{pauseflag}
          ModifyGraph fSize=8
          ModifyGraph btLen=4
          ModifyGraph axisOnTop=1
          Label right "Pauseflag"
        endif
      else // characteristic velocities not already stored, do not do the gaussian fitting
        // placeholder -- maybe want to here remove the gaussian fit, if it is present?
      endif
    else
      Print "Cannot mark transitions until fitting has started"
    endif // (StartedFitting == 1)
  else
    Print "Event 0 is reserved for unloaded"
    EventNumber = 1 // force user to look at event 1
    CheckEvent()
  endif // (EventNumber>=1)
end
//----------------------------------------------------------------------------------------------------------------------------//
macro MarkPauses()
  silent 1; pauseupdate
  variable vel1=0, vel2=0
  if (EventNumber>=1)
    if (StartedFitting == 1)
      DoWindow/F 'VelHisto'
      if (stringmatch(CsrInfo(A),"")) // if cursor A is not on the graph
        if (stringmatch(CsrInfo(B),"")) // if cursor B is also not on the graph
          Print "No cursors are on the histogram"
        else // cursor B is on graph
          Print "Only one cursor is on the histogram"
        endif
      else // cursor A is on the graph
        if (stringmatch(CsrInfo(B),"")) // cursor A is on graph but B is not
          Print "Only one cursor is on the histogram"
        else // both cursors are on the graph
          vel1=min(xcsr(A),xcsr(B))
          vel2=max(xcsr(A),xcsr(B))
          Cursor /K A
          Cursor /K B
        endif
      endif
      if (vel1!=0) // store characteristic velocities only if both cursors were on the graph
        v1[EventNumber]=vel1
        v2[EventNumber]=vel2
        FitEvent(EventNumber) // update the fits given the new characteristic velocities
      endif
    else
      Print "Cannot mark pauses until fitting has started"
    endif
  else
    Print "Event 0 is reserved for unloaded"
    EventNumber = 1 // force user to look at event 1
    CheckEvent()
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro RemovePauses()
  silent 1; pauseupdate
  v1[EventNumber] = NaN
  v2[EventNumber] =  NaN
  npexit[EventNumber] =  NaN
  tpt[EventNumber] = NaN
  npentr[EventNumber] = NaN
  trt[EventNumber] =  NaN
  DoWindow/F 'VelHisto'
  RemoveFromGraph/Z fit_sRvel_hist
  DoWindow/F 'Velocities'
  RemoveFromGraph/Z pauseflag, velatentries, velatexits
  KillWaves/Z fit_sRvel_hist
  FitEvent(EventNumber)
end
//----------------------------------------------------------------------------------------------------------------------------//
// creates an overview plot showing relative position versus time for all events
macro Overview() // this just makes it easier -- only need to type "Overview()" on command line
  silent 1 ; pauseupdate
  CreateOverview() // runs "CreateOverview()"
end
//----------------------------------------------------------------------------------------------------------------------------//
// creates an overview plot showing relative position versus time for all events
macro CreateOverview()
  silent 1; pauseupdate
  variable i=1
  variable verticaloffset=0 // used to adjust traces vertically for continuity

  dowindow/f Overview
  if(V_flag==0) // the overview graph has not previously been created
    do // iterate through all the events and place them onto the graph
      FitEvent(i)
      duplicate/o sR,$("pos"+num2str(i))
      wavestats/q $("pos"+num2str(i))
      //$("pos"+num2str(i)) -= V_min // forces minimum position to be at zero-line
      $("pos"+num2str(i)) += verticaloffset // adjusts vertical offset for continuity with previous trace
      if(term[i]>=1) // event ends with a rescue or an interruption, thus it is continued by next trace
        verticaloffset=$("pos"+num2str(i))[numpnts($("pos"+num2str(i)))-1] // raise up next trace
      else
        verticaloffset=0 // next trace will start from zero
      endif
      duplicate/o sFi,$("force"+num2str(i))
      if(i==1) // first event
        display /r $("force"+num2str(i)) // create graph and add force trace
        appendtograph $("pos"+num2str(i)) // add position trace
        dowindow/c Overview // name graph window
        modifygraph fSize=8,standoff=0, btLen=4;delayupdate
        modifygraph zero(left)=2
        label right "Force (pN)";delayupdate
        label bottom "Time (s)";delayupdate
        label left "Stage position (nm)";delayupdate
        setaxis/a/e=1 right;delayupdate
        PositionAllWindows() // moves the graphs into convenient positions for simultaneous viewing
      else // not the first event
        dowindow/f Overview // bring it back to the front
        appendtograph/r $("force"+num2str(i)) // append force trace
        appendtograph $("pos"+num2str(i)) // append position trace
      endif
      // color the traces so their boundaries can be distinguished
      if(mod(i,3)==0)
        modifygraph rgb($("force"+num2str(i)))=(48896,52992,65280),rgb($("pos"+num2str(i)))=(0,15872,65280)
      else
        if(mod(i+1,3)==0)
          modifygraph rgb($("force"+num2str(i)))=(57344,65280,48896),rgb($("pos"+num2str(i)))=(0,52224,0)
        else
          modifygraph rgb($("force"+num2str(i)))=(65280,48896,48896)
        endif
      endif
      i+=1
    while(i<=TotalEvents)
    cursor A,$("force"+num2str(EventNumber)),StartTime[EventNumber]
    cursor B,$("force"+num2str(EventNumber)),EndTime[EventNumber]
    MatchCursors()  // updates cursors on StagePos and moves cursors to appropriate waves
    FitEvent(EventNumber) // re-fit the current event so it is displayed in the EventFit graph
    dowindow/f Overview
  else // the overview graph has previously been created
    UpdateOverview()
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
// updates all the overview waves, without creating the overview graph, for use after it's been created
macro UpdateOverview()
  silent 1; pauseupdate
  variable i=1
  variable verticaloffset=0 // used to adjust traces vertically for continuity
  dowindow Overview
  if(V_flag!=0) // the overview graph has previously been created
    do // iterate through all the events and update each corresponding wave
      FitEvent(i)
      duplicate/o sR,$("pos"+num2str(i))
      wavestats/q $("pos"+num2str(i))
      //$("pos"+num2str(i)) -= V_min // forces minimum position to be at zero-line
      $("pos"+num2str(i)) += verticaloffset // adjusts vertical offset for continuity with previous trace
      if(term[i]>=1) // event ends with a rescue or an interruption, thus it is continued by next trace
        verticaloffset=$("pos"+num2str(i))[numpnts($("pos"+num2str(i)))-1] // raise up next trace
      else
        verticaloffset=0 // next trace will start from zero
      endif
      duplicate/o sFi,$("force"+num2str(i))
      dowindow/f Overview
      if(whichlistitem("force"+num2str(i),tracenamelist("",";",1),";")==-1) // trace is not already on graph
        appendtograph/r $("force"+num2str(i)) // replace with new force trace
        appendtograph $("pos"+num2str(i)) // add position trace
      endif
      // color the traces so their boundaries can be distinguished
      if(mod(i,3)==0)
        modifygraph rgb($("force"+num2str(i)))=(48896,52992,65280),rgb($("pos"+num2str(i)))=(0,15872,65280)
      else
        if(mod(i+1,3)==0)
          modifygraph rgb($("force"+num2str(i)))=(57344,65280,48896),rgb($("pos"+num2str(i)))=(0,52224,0)
        else
          modifygraph rgb($("force"+num2str(i)))=(65280,48896,48896)
        endif
      endif
      i+=1
    while(i<=TotalEvents)
    do // kill any additional waves that might contain events that were deleted or renumbered
      // note that i has not been reinitialized -- it continues where previous do-while left off
      dowindow/f Overview // bring it back to the front
      if(whichlistitem("force"+num2str(i),tracenamelist("",";",1),";")!=-1) // trace is on graph
        removefromgraph/z $("force"+num2str(i)) // remove force trace
        removefromgraph/z $("pos"+num2str(i)) // remove position trace
      endif
      killwaves/z $("force"+num2str(i)), $("pos"+num2str(i))
      i+=1
    while(i<=1002)
    cursor A,$("force"+num2str(EventNumber)),StartTime[EventNumber]
    cursor B,$("force"+num2str(EventNumber)),EndTime[EventNumber]
    MatchCursors()  // updates cursors on StagePos and moves cursors to appropriate waves
    FitEvent(EventNumber) // re-fit the current event so it is displayed in the EventFit graph
    dowindow/f Overview
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
// kills the overview plot showing relative position versus time for all events
macro KillOverview()
  silent 1; pauseupdate
  dowindow/f Overview
  if(V_flag!=0)
    killwindow Overview
    variable i=1
    do
      killwaves/z $("force"+num2str(i)), $("pos"+num2str(i))
      i+=1
    while(i<=1002)
  endif
end
//----------------------------------------------------------------------------------------------------------------------------//
macro PositionAllWindows()
  silent 1; pauseupdate
  // note:  the cardinal positions below can be changed for nicer viewing on different monitors
  // the following settings work well on Chip's surface pad
  variable x1=5,x2=440,x3=650,x4=850 // cardinal positions along horizontal axis
  variable xo=10 // horizontal offset between graphs
  variable y1=40,y2=230,y3=430,y4=510 // cardinal positions along vertical axis
  variable yo=30 // vertical offset between graphs
  // the following settings work well on Chip's laptop
  //variable x1=5,x2=510,x3=770,x4=1005 // cardinal positions along horizontal axis
  //variable xo=10 // horizontal offset between graphs
  //variable y1=40,y2=270,y3=500,y4=600 // cardinal positions along vertical axis
  //variable yo=30 // vertical offset between graphs
  dowindow/f StagePos
  if(V_flag!=0)
    movewindow x1,y1,(x2-xo),(y2-yo)
  endif
  dowindow/f BTSep
  if(V_flag!=0)
    movewindow x1,y2,(x2-xo),(y3-yo)
  endif
  dowindow/f XYplot
  if(V_flag!=0)
    movewindow (x2+3*xo),y1,(x2+3*xo+y2-y1-yo),(y2-yo)
  endif
  dowindow/f VelHisto
  if(V_flag!=0)
    movewindow x3,y1,(x3+(y2-y1-yo)),(y2-yo)
  endif
  dowindow/f EventFit
  if(V_flag!=0)
    movewindow x2,y2,x4,(y3-yo)
  endif
  dowindow/f Velocities
  if(V_flag!=0)
    movewindow x2,y3,x4,y4
  endif
  dowindow/f Overview
  if(V_flag!=0)
    movewindow (x1+2*xo),(y1+yo),(x2+2*xo),(y2-yo)
  endif
end
