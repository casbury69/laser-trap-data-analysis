#pragma rtGlobals=1		// Use modern global access method.
//-------------------------------------------------------------------------------------------------------------------//
// user selects files using dialog box, then hits cancel when all files are selected
// meanwhile the macro is concatenating all the measured properties for events in each
// file into one giant set of waves, which will allow plotting of population-wide stats
//
// The measured properties are:  F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vd,Td,Dd
//
// 10_0312 - Updated with ADF's newer Rates vs F macro and got rid of store and triage
// 18_0516 - Updated to avoid need for clicking on each file individually
//
macro compilefcdata()
  silent 1; pauseupdate
  variable i,j,done=0,num,numnewevents=0,numfilesselected=0,numnewfiles=0
  string filepath,outputpaths
  num=WaveExists($"F")+WaveExists($"Fx")+WaveExists($"Fy") // total force and components
  num+=WaveExists($"eF")+WaveExists($"eFx")+WaveExists($"eFy") // force errors
  num+=WaveExists($"thetaF")+WaveExists($"thetaS") // force and stage angles
  num+=WaveExists($"Va")+WaveExists($"Ta")+WaveExists($"Da") // vel, time and dist for ADM
  num+=WaveExists($"Vp")+WaveExists($"Tp")+WaveExists($"Dp") // vel, time and dist for pause
  num+=WaveExists($"Vd")+WaveExists($"Td")+WaveExists($"Dd") // vel, time and dist for DDM
  num+=WaveExists($"term")
  num+=WaveExists($"filenum")+WaveExists($"filename")+WaveExists($"filelist") // three extras
  if (num==21) // all waves already exist
    initial_renaming() // renames them all to have "c_" in front
  else // all twenty one waves do not already exist...
    make/O/N=0 c_F,c_Fx,c_Fy,c_eF,c_eFx,c_eFy,c_thetaF,c_thetaS
    make/O/N=0 c_Va,c_Ta,c_Da,c_Vp,c_Tp,c_Dp,c_Vd,c_Td,c_Dd,c_term,c_filenum
    make/O/N=0/T c_filename,c_filelist
  endif
  do
    open /R/D/MULT=1/T=".pxp" num // file dialog allowing multi-file selection, returns a null string if cancel is pressed
    outputpaths = s_filename
    if (strlen(outputpaths) != 0) // if files were selected
      numfilesselected = itemsinlist(outputpaths,"\r")
      i=0
      do
        killwaves /Z F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,filenum,filename,filelist
        filepath=stringfromlist(i,outputpaths,"\r")
        num=countobjects("",1) // counts number of waves
        loaddata /O/J="F;Fx;Fy;eF;eFx;eFy;thetaF;thetaS;Va;Da;Vp;Tp;Dp;Vd;Td;Dd;term" filepath
        if (num+17 == CountObjects("",1)) // if seventeen waves were successfully grabbed
          loaddata /O/J="Ta" filepath // load the Ta separately (not sure why, but loading this wave with others fails)
          make /O/N=(numpnts(F)) filenum
          make /O/N=(numpnts(F))/T filename
          stripNaNs() // removes all the NaNs that pad the imported waves
          j=0
          do // fill filenum and filename waves
            filenum[j]=numnewfiles
            filename[j]=StringFromList((ItemsInList(filepath,":")-1),filepath,":")
            j+=1
          while (j<numpnts(F))
          make /O/N=1/T filelist // one point wave for filelist
          filelist=StringFromList((ItemsInList(filepath,":")-1),filepath,":")
          concatenate_everybody() // concatenates all the newly-imported waves onto their "c_" counterparts
          numnewevents+=numpnts(F)
        else
          Print "Could not load the required waves from file: "+filepath
        endif
        i+=1
        numnewfiles+=1
      while (i<numfilesselected)
    endif
    if (YesNoPanel("MoreFiles?","No","Yes")==0)
      done=1
    endif
  while(done!=1)
  KillWaves /Z F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,filenum,filename,filelist
  final_renaming() // renames them all to remove the "c_" in front
  Print "Got "+num2str(numnewevents)+" from "+num2str(numnewfiles)+" files"
  makehistos()
end
//-------------------------------------------------------------------------------------------------------------//
// renames all the data waves so they have "c_" in front of them
macro initial_renaming()
    Rename F,c_F // then just rename them
    Rename Fx,c_Fx
    Rename Fy,c_Fy
    Rename eF,c_eF
    Rename eFx,c_eFx
    Rename eFy,c_eFy
    Rename thetaF,c_thetaF
    Rename thetaS,c_thetaS
    Rename Va,c_Va
    Rename Ta,c_Ta
    Rename Da,c_Da
    Rename Vp,c_Vp
    Rename Tp,c_Tp
    Rename Dp,c_Dp
    Rename Vd,c_Vd
    Rename Td,c_Td
    Rename Dd,c_Dd
    Rename term,c_term
    Rename filenum,c_filenum
    Rename filename,c_filename
    Rename filelist,c_filelist
end
//-------------------------------------------------------------------------------------------------------------//
// removes the "c_" from in front of all the data wave names
macro final_renaming()
    Rename c_F,F // then just rename them
    Rename c_Fx,Fx
    Rename c_Fy,Fy
    Rename c_eF,eF
    Rename c_eFx,eFx
    Rename c_eFy,eFy
    Rename c_thetaF,thetaF
    Rename c_thetaS,thetaS
    Rename c_Va,Va
    Rename c_Ta,Ta
    Rename c_Da,Da
    Rename c_Vp,Vp
    Rename c_Tp,Tp
    Rename c_Dp,Dp
    Rename c_Vd,Vd
    Rename c_Td,Td
    Rename c_Dd,Dd
    Rename c_term,term
    Rename c_filenum,filenum
    Rename c_filename,filename
    Rename c_filelist,filelist
end
//-------------------------------------------------------------------------------------------------------------//
// calculates detachment rates during assembly and disassembly
macro detrate(lowdil,hidil,lowwash,hiwash)
  variable lowdil, hidil  // limits for dilution factors to be included in calculation
  variable lowwash,hiwash // limits for washed flag to be included in calculation
  extract/o Ta,test,(term==0)&&(numtype(Td)==2)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable ndeta=numpnts(test) // number of detachments during assembly
  extract/o Ta,test,(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable tta=smartsum(test)/3600 // total time spent during assembly
  print "ndeta = "+num2str(ndeta)+"    tta = "+num2str(tta)+" hr    rdeta = "+num2str(ndeta/tta)+" ± "+num2str(sqrt(ndeta)/tta)+" per hr"
  
  extract/o Td,test,(term==0)&&(numtype(Td)==0)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable ndetd=numpnts(test) // number of detachments during assembly
  extract/o Td,test,(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable ttd=smartsum(test)/3600 // total time spent during assembly
  print "ndetd = "+num2str(ndetd)+"    ttd = "+num2str(ttd)+" hr    rdetd = "+num2str(ndetd/ttd)+" ± "+num2str(sqrt(ndetd)/ttd)+" per hr"
end
//-------------------------------------------------------------------------------------------------------------//
// estimates catastrophe and rescue rates
macro switchrate(lowdil,hidil,lowwash,hiwash)
  variable lowdil, hidil  // limits for dilution factors to be included in calculation
  variable lowwash,hiwash // limits for washed flag to be included in calculation
  extract/o Ta,test,(numtype(Ta)==0)&&(numtype(Td)==0)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable ncat=numpnts(test) // number of catastrophes
  extract/o Ta,test,(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable tta=smartsum(test)/3600 // total time spent during assembly
  print "ncat = "+num2str(ncat)+"    tta = "+num2str(tta)+" hr    rcat = "+num2str(ncat/tta)+" ± "+num2str(sqrt(ncat)/tta)+" per hr"
  
  extract/o Td,test,(term==1)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable nres=numpnts(test) // number of rescues
  extract/o Td,test,(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  variable ttd=smartsum(test)/3600 // total time spent during assembly
  print "nres = "+num2str(nres)+"    ttd = "+num2str(ttd)+" hr    rres = "+num2str(nres/ttd)+" ± "+num2str(sqrt(nres)/ttd)+" per hr"  
end
//-------------------------------------------------------------------------------------------------------------//
// determines mean growth and shortening speeds
macro meanspeed(lowdil,hidil,lowwash,hiwash)
  variable lowdil, hidil  // limits for dilution factors to be included in calculation
  variable lowwash,hiwash // limits for washed flag to be included in calculation
  extract/o Va,test,(numtype(Va)==0)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  wavestats/q test
  print "na = "+num2str(V_npnts)+"    ra = "+num2str(V_avg)+" ± "+num2str(V_sdev/sqrt(V_npnts))+" nm/s"
  
  extract/o Vd,test,(numtype(Vd)==0)&&(dilution>lowdil)&&(dilution<hidil)&&(washed>lowwash)&&(washed<hiwash)
  wavestats/q test
  print "nd = "+num2str(V_npnts)+"    rd = "+num2str(-V_avg)+" ± "+num2str(V_sdev/sqrt(V_npnts))+" nm/s"
end
//-------------------------------------------------------------------------------------------------------------//
// makes a wave, Tt, representing the total time spent attached (Ta+Tp+Td)
macro totaltime()
  duplicate/o Ta,Tt
  variable a,p,d
  variable i=0
  do
    a=0; p=0; d=0
    if(numtype(Ta[i])==0)
      a=Ta[i]
    endif
    if(numtype(Tp[i])==0)
      p=Tp[i]
    endif
    if(numtype(Td[i])==0)
      d=Td[i]
    endif
    Tt[i]=a+p+d
    i+=1
  while(i<numpnts(Tt))
end
//-------------------------------------------------------------------------------------------------------------//
// a summing routine that ignores NaNs
function smartsum(w)
  wave w
  variable i=0
  variable s=0 // initialize sum
  do
    if(numtype(w[i])==0)
      s+=w[i]
    endif
    i+=1
  while (i<numpnts(w))
  return s
end
//-------------------------------------------------------------------------------------------------------------//
// get all MT dynamic rates as function of force by binning force ranges
macro ratesVsF(numf,minf,wdf)
  variable numf 	// the number of force bins, set this to ZERO if you want to use arbitrary bin boundaries defined in wave "bb"
  variable minf 	// the minimum force to bin
  variable wdf 	// the width of force bins
  
  variable i=0 
  if (numf!=0) // if user has specified uniform bin sizes
    make/o/n=(numf+1) bb // overwrite the bin boundary wave
    do
      bb[i]=minf+i*wdf  // then fill the bb wave with regular bin boundaries
      i+=1
    while (i<=numf)
  else // user has specified arbitrary bin sizes in wave "bb"
    numf=numpnts(bb)-1
  endif
  
  // create data waves
  make/O/N=(numf) rateF,rateFer,ncat,tcat,rcat,ercat,nres,tres,rres,erres,avel,eavel,na,dvel,edvel,nd,pd,epd,pvel,epvel,np
  make/O/N=(numf) ndeta,tdeta,rdeta,erdeta,ndetd,tdetd,rdetd,erdetd,notha,totha,rotha,erotha,nothd,tothd,rothd,erothd
  make/O/N=(numf) ndet,tdet,rdet,erdet,ht,eht

  // initialization 
  rateF=NaN
  rateFer=NaN
  ncat=0
  tcat=NaN
  rcat=NaN
  ercat=NaN
  nres=0
  tres=NaN
  rres=NaN
  erres=NaN
  avel=NaN
  eavel=NaN
  na=0
  dvel=NaN
  edvel=NaN
  nd=0
  pd=NaN
  epd=NaN
  pvel=NaN
  epvel=NaN
  np=0
  ndeta=0
  tdeta=NaN
  rdeta=NaN
  erdeta=NaN
  ndetd=0
  tdetd=NaN
  rdetd=NaN
  erdetd=NaN
  notha=0
  totha=NaN
  rotha=NaN
  erotha=NaN
  nothd=0
  tothd=NaN
  rothd=NaN
  erothd=NaN
  ndet=0
  tdet=NaN
  rdet=NaN
  erdet=NaN
  ht=NaN
  eht=NaN

  // big do loop begins here
  i=0
  do
  
    extract/O F, temp, (F >= bb[i] && F < bb[i+1])  // extract force values
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp   
      if (V_npnts>0)
	  //rateF[i]=minf+i*wdf+wdf/2 // center of force bin
	  //rateFer[i]=wdf/2 // half-width of force bin
	  rateF[i]=V_avg // mean force
	  rateFer[i]=V_sdev // s.d. of force distribution
      endif
    endif   
    killwaves temp
    
    extract/O Vp, temp, (F >= bb[i] && F < bb[i+1])  // extract pause "velocities"
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 //////////////////////////////////////////////////////////////////////// 
      wavestats/q temp
      if (V_npnts>0)
        np[i]=V_npnts // number of pauses equals number of actual values in Vp wave
        pvel[i]=V_avg // mean pause "velocity" -- idea is to check that its nearly zero
        epvel[i]=V_sdev/sqrt(V_npnts-1) // sem for pause "velocity"
      endif
    endif
    killwaves temp
      
    extract/O Va, temp, (F >= bb[i] && F < bb[i+1])  // extract assembly velocities
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
    wavestats/q temp
      if (V_npnts>0)
        na[i]=V_npnts // number of ADMs equals number of actual values in Va wave
        avel[i]=V_avg // mean ADM speed
        eavel[i]=V_sdev/sqrt(V_npnts-1) // sem for ADM speed
      endif    
    endif  
    killwaves temp
    
    extract/O Vd, temp, (F >= bb[i] && F < bb[i+1]) // extract disassembly velocities
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
    wavestats/q temp
      if (V_npnts>0)
 	    nd[i]=V_npnts // number of DDMs equals number of actual values in Vd wave
	    dvel[i]=V_avg // mean DDM speed
	    edvel[i]=V_sdev/sqrt(V_npnts-1) // sem for DDM speed
	endif    
    endif
    killwaves temp
    
    extract/O Tp, temp, (F >= bb[i] && F < bb[i+1])  // extract pause durations
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        pd[i]=V_avg // mean pause duration
        epd[i]=V_sdev/sqrt(V_npnts-1) // sem for pause duration
      endif
    endif
    killwaves temp
   
    extract/O Ta, temp, (F >= bb[i] && F < bb[i+1])  // extract assembly durations
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        tcat[i]=V_sum/3600 //total time of ADM (pause not included) in hours
        if (numtype(pd[i])==0)
          tcat[i]=(V_sum+pd[i])/3600 //total time of ADM (pause included) in hours
        endif
      else
        tcat[i]=pd[i])/3600
      endif
    endif
    killwaves temp
    
    extract/O Td, temp, (F >= bb[i] && F < bb[i+1])  // extract disassembly durations
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        tres[i]=V_sum/3600 //total time of DDM in hours
      endif
    endif
    killwaves temp

    // count catastrophes only when events have both ADM and DDM
    extract/O Ta, temp, (F >= bb[i] && F < bb[i+1] && numtype(Ta)==0 && numtype(Td)==0)
    if (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        ncat[i]=V_npnts
      endif
    endif
    killwaves temp
    rcat[i]=ncat[i]/tcat[i] // catastrophe rate in hrs**-1
    ercat[i]=sqrt(ncat[i])/tcat[i] // error based on sqrt of N

    extract/O term, temp, (F >= bb[i] && F < bb[i+1] && term==1)  // count rescues
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        nres[i] = V_npnts
      endif
    endif
    killwaves temp  
    rres[i]=nres[i]/tres[i] // rescue rate in hrs**-1
    erres[i]=sqrt(nres[i])/tres[i] // error based on sqrt of N
    
  // count detachments that occurred during ADM
    extract/O Ta, temp, (F >= bb[i] && F < bb[i+1] && term==0 && numtype(Td)==2)
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        ndeta[i]=V_npnts // number of detachments during ADM
      endif
    endif
    killwaves temp
    tdeta[i]=tcat[i] // total time of ADM in hours calculated above including all events
    rdeta[i]=ndeta[i]/tdeta[i] // rate of detachment during ADM in hrs**-1
    erdeta[i]=sqrt(ndeta[i])/tdeta[i] // error in rate of detachment during ADM based on sqrt N

    // count detachments that occurred during DDM
    extract/O Td, temp, (F >= bb[i] && F < bb[i+1] && term==0)
    if  (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        ndetd[i]=V_npnts // number of detachments during DDM
      endif
    endif
    killwaves temp
    tdetd[i]=tres[i] // total time of DDM in hours calculated above including all events
    rdetd[i]=ndetd[i]/tdetd[i] // rate of detachment during DDM in hrs**-1
    erdetd[i]=sqrt(ndetd[i])/tdetd[i] // error in rate of detachment during DDM based on sqrt N
    
    // count detachments that occured either during ADM or DDM
    ndet[i]+=ndeta[i]+ndetd[i]
    if (numtype(tdeta[i])==0 && numtype(tdetd[i])==0) // observations include both ADM and DDM time 
      tdet[i]=tdeta[i]+tdetd[i]
    else // either ADM or DDM observation time (or both) is NaN
      if (numtype(tdeta[i])==0 && numtype(tdetd[i])!=0) // observations include only ADM time
        tdet[i]=tdeta[i]
        if (numtype(tdeta[i])!=0 && numtype(tdetd[i])==0) // observations include only DDM time
          tdet[i]=tdetd[i]
        endif
      endif
    endif
    rdet[i]=ndet[i]/tdet[i]  // rate of detachment in hrs**-1
    erdet[i]=sqrt(ndet[i])/tdet[i]  // error in rate of detachment based on sqrt N
    ht[i]=60/rdet[i]  // hold time in minutes
    eht[i]=ht[i]*erdet[i]/rdet[i]  // error in hold time assuming same fractional error as detachment rate
    
    // count events that were terminated by "other" during ADM
    extract/o Ta, temp, (F >= bb[i] && F < bb[i+1] && numtype(term)==2 && numtype(Td)==2)
    if (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        notha[i]=V_npnts // number of events terminated by "other" during ADM
        totha[i]=tcat[i] // total time of ADM in hours calculated above including all events
        rotha[i]=notha[i]/totha[i] // rate of "other" during ADM in hrs**-1
        erotha[i]=sqrt(notha[i])/totha[i] // error in rate of "other" during ADM based on sqrt N
      endif
    endif
    killwaves temp

    // count events that were terminated by "other" during DDM
    extract/o Td, temp, (F >= bb[i] && F < bb[i+1] && numtype(term)==2)
    if (numpnts(temp)>0) // chip and matt changed this on 15_1130 ////////////////////////////////////////////////////////////////////////
      wavestats/q temp
      if (V_npnts>0)
        nothd[i]=V_npnts // number of events terminated by "other" during DDM
        tothd[i]=tres[i] // total time of DDM in hours calculated above including all events
        rothd[i]=nothd[i]/tothd[i] // rate of "other" during DDM in hrs**-1
        erothd[i]=sqrt(nothd[i])/tothd[i] // error in rate of "other" during DDM based on sqrt N
      endif
    endif
    killwaves temp
    
    i+=1
    while (i<numf)
    
    dvel*=-1 // to make the disassembly velocity a positive number

    edit rateF,rateFer,ncat,tcat,rcat,ercat,nres,tres,rres,erres,avel,eavel,na,dvel,edvel,nd,pd,epd,pvel,epvel,np,ndeta,tdeta,rdeta,erdeta,ndetd,tdetd,rdetd,erdetd,ndet,tdet,rdet,ht,eht,erdet,notha,totha,rotha,erotha,nothd,tothd,rothd,erothd
  
    // generate plots of various things versus force
    display avel vs rateF
    errorbars/T=0.5/L=0.5 avel XY,wave=(rateFer,rateFer),wave=(eavel,eavel)
    label left "Growth speed (nm s\\S-1\\M)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

    display dvel vs rateF
    errorbars/T=0.5/L=0.5 dvel XY,wave=(rateFer,rateFer),wave=(edvel,edvel)
    label left "Shortening speed (nm s\\S-1\\M)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

    display rcat vs rateF
    errorbars/T=0.5/L=0.5 rcat XY,wave=(rateFer,rateFer),wave=(ercat,ercat)
    label left "Catastrophe frequency (hr\\S-1\\M)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

    display rres vs rateF
    errorbars/T=0.5/L=0.5 rres XY,wave=(rateFer,rateFer),wave=(erres,erres)
    label left "Rescue frequency (hr\\S-1\\M)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

    display rdeta vs rateF
    errorbars/T=0.5/L=0.5 rdeta XY,wave=(rateFer,rateFer),wave=(erdeta,erdeta)
    label left "Detachment frequency (hr\\S-1\\M)"
    label bottom "Force (pN)"
    appendtograph rdetd vs rateF
    errorbars/T=0.5/L=0.5 rdetd XY,wave=(rateFer,rateFer),wave=(erdetd,erdetd)
    modifygraph log(left)=1;DelayUpdate
    modifygraph mode=4,marker=8
    
    display rdet vs rateF
    errorbars/T=0.5/L=0.5 rdet XY,wave=(rateFer,rateFer),wave=(erdet,erdet)
    label left "Detachment frequency (hr\\S-1\\M)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

    display ht vs rateF
    errorbars/T=0.5/L=0.5 ht XY,wave=(rateFer,rateFer),wave=(eht,eht)
    label left "Hold time (min)"
    label bottom "Force (pN)"
    modifygraph mode=4,marker=8

end 
//-------------------------------------------------------------------------------------------------------------//
// concatenates the data waves without "c_" to those with
macro concatenate_everybody()
    ConcatenateWaves(c_F,F)
    ConcatenateWaves(c_Fx,Fx)
    ConcatenateWaves(c_Fy,Fy)
    ConcatenateWaves(c_eF,eF)
    ConcatenateWaves(c_eFx,eFx)
    ConcatenateWaves(c_eFy,eFy)
    ConcatenateWaves(c_thetaF,thetaF)
    ConcatenateWaves(c_thetaS,thetaS)
    ConcatenateWaves(c_Va,Va)
    ConcatenateWaves(c_Ta,Ta)
    ConcatenateWaves(c_Da,Da)
    ConcatenateWaves(c_Vp,Vp)
    ConcatenateWaves(c_Tp,Tp)
    ConcatenateWaves(c_Dp,Dp)
    ConcatenateWaves(c_Vd,Vd)
    ConcatenateWaves(c_Td,Td)
    ConcatenateWaves(c_Dd,Dd)
    ConcatenateWaves(c_term,term)
    ConcatenateWaves(c_filenum,filenum)
    ConcatenateTextWaves(c_filename,filename)
    ConcatenateTextWaves(c_filelist,filelist)
end
//-------------------------------------------------------------------------------------------------------------//
// just removes all points containing NaN from the data waves
macro stripNaNs()
  silent 1
  pauseupdate
  variable i=numpnts(F)
  do
    if(numtype(F[i])==2) // if F[i] is NaN
      DeletePoints i,1,F,Fx,Fy,eFx,eFy,eF,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,filenum,filename
    endif
    i-=1
  while(i>=0)
end
//-------------------------------------------------------------------------------------------------------------//
// routine to automatically make histograms
macro makehistos()
  if(numpnts(F)>0)
    variable leftedge = 910
    variable topedge = 40
    variable height = 130
    variable width = 130
    variable vertoffset = height + 25
    string dumstring
    make /o/n=2 vlinex,vliney
    vlinex[0]=0
    vlinex[1]=0
    vliney[0]=0
    vliney[1]=1000
    // Table of data
    DoWindow/F MsmntTable
    if (V_flag == 0) // if table does not already exist
      Edit F,Fx,Fy,eF,eFx,eFy,thetaF,thetaS,Va,Ta,Da,Vp,Tp,Dp,Vd,Td,Dd,term,filenum,filename,filelist
      DoWindow /C $"MsmntTable"
      MoveWindow 10,40,800,500
      ModifyTable size=8,width=40
    endif
    // Force histogram---------------------------------------------------
    DoWindow/F ForceHisto
    if (V_flag == 0) // if histogram does not already exist
      Make/N=6/D/O F_Hist;DelayUpdate
      Histogram/B={0,0.2,40} F,F_Hist
      Display F_Hist
      SetAxis/A; ModifyGraph manTick=0; ModifyGraph grid=0
      Label left "Number"; Label bottom "Force (pN)"
      ModifyGraph rgb=(65280,0,0); ModifyGraph width=0,height=0
      AppendToGraph vliney vs vlinex
      ModifyGraph lstyle(vliney)=1
      wavestats/Q F
      ModifyGraph offset(vliney)={(V_avg),0}
      sprintf dumstring,"\\Z08%3.1f ± %3.1f pN",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      SetAxis left 0,(V_npnts/4)
      ModifyGraph mode(F_Hist)=6,rgb(vliney)=(0,0,0),rgb(F_Hist)=(65280,0,0)
      ModifyGraph tick=2,mirror=2,fSize=8,standoff=0
      MoveWindow (leftedge),(topedge),(leftedge+width),(topedge+height)
      DoWindow /C $"ForceHisto"
    else // if histogram already exists
      Histogram/B={0,0.2,40} F,F_Hist
      wavestats/Q F
      ModifyGraph offset(vliney)={(V_avg),0}
      sprintf dumstring,"\\Z08%3.1f ± %3.1f pN",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      SetAxis left 0,(V_npnts/4)
    endif
    // ADM velocity histogram----------------------------------------
    DoWindow/F ADMvelHisto
    if (V_flag == 0) // if histogram does not already exist
      Make/N=6/D/O Va_Hist;DelayUpdate
      Histogram/B={0,2,7} Va,Va_Hist
      Display Va_Hist
      SetAxis/A; ModifyGraph manTick=0; ModifyGraph grid=0
      Label left "Number"; Label bottom "ADM speed (nm s\\S-1\\M)"
      ModifyGraph rgb=(65280,0,0); ModifyGraph width=0,height=0
      wavestats/Q Va
      AppendToGraph vliney vs vlinex
      ModifyGraph offset(vliney)={(V_avg),0}
      ModifyGraph lstyle(vliney)=1
      sprintf dumstring,"\\Z08%3.1f ± %3.1f nm s\\S-1",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      SetAxis left 0,(V_npnts/2)
      ModifyGraph mode(Va_Hist)=6,rgb(vliney)=(0,0,0),rgb(Va_Hist)=(65280,0,0)
      ModifyGraph tick=2,mirror=2,fSize=8,standoff=0
      MoveWindow (leftedge),(topedge+vertoffset),(leftedge+width),(topedge+vertoffset+height)
      DoWindow /C $"ADMvelHisto"
    else
      Histogram/B={0,2,7} Va,Va_Hist
      wavestats/Q Va
      ModifyGraph offset(vliney)={(V_avg),0}
      sprintf dumstring,"\\Z08%3.1f ± %3.1f nm s\\S-1",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      SetAxis left 0,(V_npnts/2)
    endif
    // ADM distance histogram---------------------------------------------------
    DoWindow/F ADMdistHisto
    if (V_flag == 0) // if histogram does not already exist
      Make/N=6/D/O Da_Hist;DelayUpdate
      Histogram/B={0,100,40} Da,Da_Hist
      Display Da_Hist
      SetAxis/A; ModifyGraph manTick=0; ModifyGraph grid=0
      Label left "Number"; Label bottom "ADM distance (nm)"
      ModifyGraph rgb=(65280,0,0); ModifyGraph width=0,height=0
      wavestats/Q Da
      AppendToGraph vliney vs vlinex
      ModifyGraph offset(vliney)={(V_avg),0}
      ModifyGraph lstyle(vliney)=1
      sprintf dumstring,"\\Z08%3.0f ± %3.0f nm",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      ModifyGraph log(left)=1
      SetAxis left 1,(V_npnts)
      ModifyGraph mode(Da_Hist)=6,rgb(vliney)=(0,0,0),rgb(Da_Hist)=(65280,0,0)
      ModifyGraph tick=2,mirror=2,fSize=8,standoff=0
      MoveWindow (leftedge),(topedge+vertoffset*2),(leftedge+width),(topedge+vertoffset*2+height)
      DoWindow /C $"ADMdistHisto"
    else
      Histogram/B={0,100,40} Da,Da_Hist
      wavestats/Q Da
      ModifyGraph offset(vliney)={(V_avg),1}
      sprintf dumstring,"\\Z08%3.0f ± %3.0f nm",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      ModifyGraph log(left)=1
      SetAxis left 1,(V_npnts)
    endif
    // ADM time histogram
    DoWindow/F ADMtimeHisto
    if (V_flag == 0) // if histogram does not already exist
      Make/N=6/D/O Ta_Hist;DelayUpdate
      Histogram/B={0,100,20} Ta,Ta_Hist
      Display Ta_Hist
      SetAxis/A; ModifyGraph manTick=0; ModifyGraph grid=0
      Label left "Number"; Label bottom "ADM time (s)"
      ModifyGraph rgb=(65280,0,0); ModifyGraph width=0,height=0
      wavestats/Q Ta
      AppendToGraph vliney vs vlinex
      ModifyGraph offset(vliney)={(V_avg),1}
      ModifyGraph lstyle(vliney)=1
      sprintf dumstring,"\\Z08%3.0f ± %3.0f s",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      ModifyGraph log(left)=1
      SetAxis left 1,(V_npnts)
      ModifyGraph mode(Ta_Hist)=6,rgb(vliney)=(0,0,0),rgb(Ta_Hist)=(65280,0,0)
      ModifyGraph tick=2,mirror=2,fSize=8,standoff=0
      MoveWindow (leftedge),(topedge+vertoffset*3),(leftedge+width),(topedge+vertoffset*3+height)
      DoWindow /C $"ADMtimeHisto"
    else
      Histogram/B={0,100,20} Ta,Ta_Hist
      wavestats/Q Ta
      ModifyGraph offset(vliney)={(V_avg),0}
      sprintf dumstring,"\\Z08%3.0f ± %3.0f s",V_avg,V_sdev
      TextBox/C/N=text0/F=0/A=MC dumstring
      ModifyGraph log(left)=1
      SetAxis left 1,(V_npnts)
    endif
  endif
end
//-------------------------------------------------------------------------------------------------------------//
// Josh's Yes No Panel copied below
Function YesNoPanel(InputString,button0Name,button1Name)	
	// Makes an Yes/No dialog box.  
	// Returns 0 if button0 is pressed,
	//             1 if button1 is pressed
	String InputString,button0Name,button1Name	// Dialog text, and name of two buttons	
	NewPanel/k=2/W=(600,300,800,400) //left,top,right,bottom
	DoWindow/C DialogPanel // renames the window "DialogPanel"
	SetDrawEnv xcoord= rel,ycoord= rel,textxjust= 1,textyjust= 1 // use relative coords and center justify text
	DrawText 0.5,0.2,InputString // draws the text with relative coordinates
	Button button0 pos={20,60},size={60,20},proc=Button0,title=button0Name
	Button button1 pos={110,60},size={60,20},proc=Button1,title=button1Name
	variable/G whichButton = NaN // create a global variable to remember which button is pressed
	PauseForUser DialogPanel // pauses operation until the "Dialog Panel" window is killed
	variable temp = whichButton // stores state of whichButton global variable in local temp variable
	Killvariables/Z whichButton // kills the global variable
	print "got the following:  "+num2str(temp)
	return temp // returns the number of the button that got pressed
End
Function Button0(ctrlName) : ButtonControl
String ctrlName
	NVAR whichButton // local definition of global variable
	whichButton = 0 // set global variable 
	DoWindow/K DialogPanel // kill the window "DialogPanel"
End
Function Button1(ctrlName) : ButtonControl
String ctrlName
	NVAR whichButton
	whichButton = 1
	DoWindow/K DialogPanel
End
//-------------------------------------------------------------------------------------------------------------//
// compute survival probability versus time and distance
macro survival()
  silent 1
  pauseupdate
  variable i=0
  variable Dholder=0
  variable Tholder=0
  duplicate/o Da,Dtot,Ttot,Ftot
  do
  
    Ftot[i]=F[i]
    Dtot[i]=Dholder // first add distances for assembly, pause, and disassembly
    if (numtype(Da[i])!=2) // if Da is not NaN
      Dtot[i]+=Da[i]
    endif
    if (numtype(Dd[i])!=2) // if Dd is not NaN
      Dtot[i]+=-1*Dd[i] // negation because disassembly is negative
    endif
    if (numtype(Dp[i])!=2) // if Dp is not NaN
      Dtot[i]+=Dp[i]
    endif
    
    Ttot[i]=Tholder // then add times for assembly, pause, and disassembly
    if (numtype(Ta[i])!=2) // if Ta is not NaN
      Ttot[i]+=Ta[i]
    endif
    if (numtype(Td[i])!=2) // if Td is not NaN
      Ttot[i]+=Td[i]
    endif
    if (numtype(Tp[i])!=2) // if Tp is not NaN
      Ttot[i]+=Tp[i]
    endif
    
    if (term[i]==2 || term[i]==1) // if this event was interrupted, or if it ended in rescue
      Dholder=Dtot[i] // carry sums forward to next row
      Dtot[i]=NaN
      Tholder=Ttot[i]
      Ttot[i]=NaN
    else
      Dholder=0 // otherwise no carry-forward
      Tholder=0
    endif

    i+=1
  while (i<numpnts(Da))
  
  i=numpnts(Dtot) // now strip any NaNs, potentially reducing the length of the waves
  do
    if(numtype(Dtot[i])==2) // if Dtot[i] is NaN
      DeletePoints i,1,Dtot,Ttot,Ftot
    endif
    i-=1
  while(i>=0)
  
//  duplicate/o Dtot,sDtot,sTtot // waves to hold fraction surviving
//  sort/r Dtot,Dtot
//  sDtot=p/(numpnts(sDtot)-1)
//  sort/r Ttot,Ttot
//  sTtot=p/(numpnts(sTtot)-1)
end