#pragma rtGlobals=1		// Use modern global access method.
// This hook function moves the A and B cursors with just a single mouse click
// Moves cursor A if <Ctrl> key is held
// Moves cursor B if <Shift> key is held
// If click is not near any traces, then the cursors may end up at the origin!
// To use this function, you must execute the following command, which tells IGOR to call this function upon mouse clicks:
//
//     "SetWindow Graph0, hook=QuickCursors, hookcursor=0, hookEvents=1"
//
// Replace "Graph0" with the name of the graph window in which you want to have quick cursors
// You also need to have the GetInputState.xop extension turned on (by placing a link to it in Igor Extensions folder)

Function QuickCursors(infoStr)
	String infoStr

	String event = StringByKey("EVENT",infoStr)
	if(stringmatch(event,"mouseup")) // if mouseup
	  String mouseXstring = StringByKey("MOUSEX",infoStr)
	  String mouseYstring = StringByKey("MOUSEY",infoStr)
	  Variable mouseXpos = str2num(mouseXstring)
	  Variable mouseYpos = str2num(mouseYstring)
	  String traceInfoStr = TraceFromPixel(mouseXpos,mouseYpos,"") // translates mouse coords to a point on a trace!!!!!!!!!
 	  String traceNameStr = StringByKey("TRACE",traceInfoStr)
	  Variable traceHitPoint = str2num(StringByKey("HITPOINT",traceInfoStr))
	  String keyBoardStr = keyBoardState("") // returns a string indicating what keys are currently pressed
	  //Print UpperStr(keyBoardStr[9]) // note: element 9 contains the first character that is currently pressed
	  if (cmpstr(keyBoardStr[0],"1") == 0) // note: cmpstr returns 0 if the strings are equivalent
	    // if <Ctrl> key was held, update cursor A
	    Cursor /P A $traceNameStr (traceHitPoint) // with /P flag, hitPoint is interpretted correctly as point number
	  else
	    if (cmpstr(keyBoardStr[3],"1") == 0) 
	      // if <Shift> key was held, update cursor B
	      Cursor /P B $traceNameStr (traceHitPoint) // with /P flag, hitPoint is interpretted correctly as point number
	    else
	    	if (cmpstr(keyBoardStr[4],"1") == 0)
	    	  // if <caps lock> key was held, print quit
	    	  Print "Quit"
	    	endif // Quit
	    endif // <Shift>
	  endif // <Ctrl>
      endif // mouseup
	return 0 // returns a zero meaning nothing has been done
End
