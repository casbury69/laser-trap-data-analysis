#pragma rtGlobals=3		// Use modern global access method and strict wave access.

//--------------------------------------------------------------------------------------------------------------------------------------------------------------//
// CLA modified 17_0524 to provide a hand-select option for the beginning of stall
macro stallquant()
  variable smwin = 1000 // number of points in smoothing window (500 pts = 2.5 sec) originally used 400 points
  variable velwin = 500 // number of points in fitting window for estimation of speed (200 pts = 1 sec) originally used 200 points
  variable thresh = 2 // speed in nm/s above which bead is considered moving, originally used 10 nm/s
  variable fitstartp, fitendp // start and end points for the speed fitting window
  variable detachp, stallp //detach point is the point at which the dynein detaches. Stallp is the point at which the dynein bead becomes stalled
  variable dumnum
  
  duplicate /o bXi,sepX // temporary wave for  bead-trap sep or force along x
  duplicate /o bYi,sepY // temporary wave for bead-trap sep or force along y
  duplicate /o bYi, sepR // vector distance, bead-trap sep
  wavestats/q/r=[0,10] sepX
  sepX-=V_avg // subtracts initial x pos, so stage pos is relative to start of event
  wavestats/q/r=[0,10] sepY
  sepY-=V_avg // subtracts initial y pos
  sepR=rotX( sepX, sepY, (-1*thetaF[EventNumber]) )
  duplicate /o sepR, ssepR
  //smooth/b=2 (smwin), ssepR //smoothing with a window of smwin 
  duplicate /o ssepR, ssepR_pos // remember the smoothed sep trace prior to differentiation
  differentiate ssepR // estimating instantaneous velocity vs time

  dowindow/F EventFit //brings the Event window to the front
  if(findlistitem("sepR",tracenamelist("EventFit",";",1))==-1) // if the wave "sepR" is not found on EventFit graph, then graph needs updating for open-loop fitting
    appendtograph sepR  // show the bead-trap sep data instead of the stage motion which is flatline for openloop data
    setaxis right -6,6
    setaxis left -50,300
    modifygraph mode(sepR)=0
    modifygraph rgb(sepR)=(47872,47872,47872)
  else
    removefromgraph/z fit_sepR_stall, fit_sepR_speed
    killwaves/z fit_sepR_stall, fit_sepR_speed
  endif

  if (numtype(CatTime[EventNumber]) == 0) // user has already indicated a "CatTime" which here means time of detachment
    cursor A Fi (CatTime[EventNumber])
    detachp=pcsr(A)
    // older code for automatically finding the stall point by searching backward to find where velocity is above threshold.....................................
    //findlevel/q/p/r=[(pcsr(A)-0.5*smwin),0] ssepR, (thresh) //searches backwards to find the point where the velocity is above thresh
    //stallp=V_levelx
    //cursor/p B, sFi, stallp //place cursor b on same graph at stall point
    // new code to allow hand-selection of stall point...................................................................................................................................................  
    cursor B Fi (PauseTime[EventNumber])
    stallp=pcsr(B)
    CatTime[EventNumber]=pnt2x(ssepR, detachp) //convert the detach point to x value and store in table
    PauseTime[EventNumber]=pnt2x(ssepR, stallp) //convert the stall point to x value and store in table
    Tp[EventNumber]=CatTime[EventNumber]-PauseTime[EventNumber]
    curvefit/q/NTHR=0 line sepR[pcsr(B),pcsr(A)] /D; DelayUpdate  // line fits to get velocity during pause
    modifygraph rgb(fit_sepR)=(0,39168,0)
    rename fit_sepR fit_sepR_stall
    Vp[EventNumber] = W_coef[1]
    wavestats/q/r=(PauseTime[EventNumber],CatTime[EventNumber]) Fi // gets the mean and SEM of force during pause interval
    F[EventNumber] = V_avg
    eF[EventNumber] = V_sem
    //findlevel/q/p/r=[pcsr(B),0] ssepR_pos, (0.75*ssepR_pos[pcsr(B)]) // define limits for velocity fitting, end at 75% displacement
    //fitendp = V_LevelX
    //if (V_flag==1)
      fitendp = pcsr(B) // in case findlevel pukes, this sets the end of fit to the start of stall
    //endif
    //findlevel/q/p/r=[pcsr(B),0] ssepR_pos, (0.25*ssepR_pos[pcsr(B)]) // define limits for velocity fitting, start at 25% displacement
    //fitstartp = V_LevelX
    //if (V_flag==1) // in case findlevel pukes, this sets the start of fit to the start of the event
      fitstartp = 0
    //endif
//    findlevel/q/p/r=[pcsr(B),0] sepR, (0.5*sepR[pcsr(B)]) // older code with velocity window of fixed time-width
//    fitendp = V_LevelX+0.5*velwin
//    fitstartp = V_LevelX-0.5*velwin
//    findlevel/q/p/r=[pcsr(B),0] ssepR_pos, (0.5*ssepR_pos[pcsr(B)])
//    make/o/n=2 coefficientwave
//    coefficientwave={0,ssepR[V_LevelX]} // this essentially constrained the velocity fit to have slope from ssepR at 50% displacement
//    curvefit/q/h="01"/NTHR=0 line, kwCWave=coefficientwave, sepR[fitstartp,fitendp] /D; DelayUpdate  // constrained curve fit
    curvefit/q/NTHR=0 line, sepR[fitstartp,fitendp] /D; DelayUpdate  // line fits to get velocity during non-pause
    modifygraph rgb(fit_sepR)=(0,0,65280)
    rename fit_sepR fit_sepR_speed
    Va[EventNumber] = W_coef[1]
  else // user has not indicated a "CatTime"
    CatTime[EventNumber]=NaN
    PauseTime[EventNumber]=NaN
    Vd[EventNumber] = NaN
    Td[EventNumber] = NaN
    Dd[EventNumber] =  NaN
    Vp[EventNumber] = NaN
    Tp[EventNumber] = NaN
    Dp[EventNumber] =  NaN
    Va[EventNumber] = NaN
    Ta[EventNumber] = NaN
    Da[EventNumber] =  NaN
    Fx[EventNumber] = NaN
    eFx[EventNumber] = NaN
    Fy[EventNumber] = NaN
    eFy[EventNumber] = NaN
    F[EventNumber] = NaN
    eF[EventNumber] = NaN    
    cursor /k A
    cursor /k B
  endif

end
//--------------------------------------------------------------------------------------------------------------------------------------------------------------//
macro markstall()
  // CatTime[EventNumber] = xcsr(A) // old code, start of stall was found automatically by searching backwards for vel above thresh
  // newer code for hand-selection of start of stall
  CatTime[EventNumber] = max(xcsr(A),xcsr(B))
  PauseTime[EventNumber] = min(xcsr(A),xcsr(B))
  FitEvent(EventNumber) // update the fit given the new CatTime
end
//--------------------------------------------------------------------------------------------------------------------------------------------------------------//
macro removestall()
  CatTime[EventNumber] = NaN
  FitEvent(EventNumber) // update the fit given the removed CatTime
end