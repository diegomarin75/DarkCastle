' Draw isometric line
SUB ISOLine (a1 AS INTEGER, b1 AS INTEGER, c1 AS INTEGER, _
             a2 AS INTEGER, b2 AS INTEGER, c2 AS INTEGER, col AS UINTEGER)
void bresline( int x1,int y1,int x2,int y2,
     unsigned char color,unsigned char far *screen ){

  'Variables
  DIM AS INTEGER px1, px2
  DIM AS INTEGER py1, py2
  DIM AS INTEGER xdiff, ydiff
  DIM AS INTEGER length, error_term
  DIM AS INTEGER ax, ay, i
  
  'Proyect line
  _ISODTrans(a1,b1,c1,px1,py1)
  _ISODTrans(a2,b2,c2,px2,py2)
  
  'Init loop variables
  error_term = 0
  IF xdiff >= 0 THEN: xdiff=px2-px1+1: ax=1  : ELSE: xdiff=px1-px2+1: ax=-1  : ENDIF
  IF ydiff >= 0 THEN: ydiff=px2-px1+1: ay=AX0: ELSE: ydiff=px1-px2+1: ay=-AX0: ENDIF

  IF xdiff >= ydiff THEN
   length = xdiff
   FOR i=0 TO length-1
     screen[offset]=color;
     _ISOZBufferDist(a,b,c,x,y,plane,dist)
     _ISOLightCalc(a,b,c,x,y,plane,light)
     _ISOModulateColor(col,light,colm)
     _ISOPixel(px,py,col,colm,dist)

     
     error_term+=ydiff;
     if( error_term>=xdiff ){ offset+=ay; error_term-=xdiff; }
     offset+=ax;
   }
 }

 if( xdiff<ydiff ){
   len=ydiff;
   for( i=0;i<len;i++ ){
     screen[offset]=color;
     error_term+=xdiff;
     if( error_term>=ydiff ){ offset+=ax; error_term-=ydiff; }
     offset+=ay;
   }
 }

