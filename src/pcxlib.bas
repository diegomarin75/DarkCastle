#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"
#INCLUDE "vbcompat.bi"

'Image display in High color modes
SUB ImgDisplay(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, ipt AS UINTEGER PTR, mirror AS INTEGER)
  
  'Variables
  DIM AS INTEGER i, j
  DIM AS UINTEGER pxcol
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    pxcol = ipt[(ax * j) + i]
    IF pxcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        PSET (x0 + i, y0 + j), pxcol
      ELSE
        PSET (x0 + ax - 1 - i, y0 + j), pxcol
      ENDIF
    ENDIF
  NEXT j
  NEXT i
  
END SUB

'Image display in High color modes with size limit
SUB ImgDisplayClip(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
                   ipt AS UINTEGER PTR, mirror AS INTEGER, sax AS INTEGER, say AS INTEGER)
  
  'Variables
  DIM AS INTEGER i, j
  DIM AS UINTEGER pxcol
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    pxcol = ipt[(ax * j) + i]
    IF pxcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        IF i < sax AND j < say THEN PSET (x0 + i, y0 + j), pxcol
      ELSE
        IF ax - 1 - i < sax AND j < say THEN PSET (x0 + ax - 1 - i, y0 + j), pxcol
      ENDIF
    ENDIF
  NEXT j
  NEXT i
  
END SUB

'Image display in High color modes with color modulation
SUB ImgDisplayMod(x0 AS INTEGER, y0 AS INTEGER, ax AS INTEGER, ay AS INTEGER, _
                  ipt AS UINTEGER PTR, mirror AS INTEGER, modcol AS UINTEGER)
  
  'Color modulation formula
  #MACRO _Modulate(imgcolor,scrcolor,modcolor,result)
    result = (((modcolor)-(scrcolor))*(imgcolor))/255+(scrcolor)
  #ENDMACRO
  
  'Variables
  DIM AS INTEGER i, j, px, py
  DIM AS UINTEGER imgcol,scrcol
  DIM AS UINTEGER r,g,b
  
  'Loop (normal mode)
  FOR i = 0 TO ax - 1
  FOR j = 0 TO ay - 1
    imgcol = ipt[(ax * j) + i]
    IF imgcol <> TRANCOLHI THEN 
      IF mirror = 0 THEN
        px = x0 + i: py = y0 + j
      ELSE
        px = x0 + ax - 1 - i: py = y0 + j
      ENDIF
      scrcol = POINT(px,py)
      _Modulate(RGB_R(imgcol),RGB_R(scrcol),RGB_R(modcol),r)
      _Modulate(RGB_G(imgcol),RGB_G(scrcol),RGB_G(modcol),g)
      _Modulate(RGB_B(imgcol),RGB_B(scrcol),RGB_B(modcol),b)
      PSET (px,py), RGB(r,g,b)
    ENDIF
  NEXT j
  NEXT i
  
END SUB

' Load PCX file into memory buffer (for High color modes)                               
SUB PcxLoad (filename AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, _
             img() AS UINTEGER, xlim AS INTEGER, ylim AS INTEGER, trans AS INTEGER)
                                                                            
  'Variables                                                     
  DIM AS INTEGER file                  'File
  DIM AS INTEGER i                     'Counter
  DIM AS INTEGER asci                  'ASCII value
  DIM AS LONG Size, l, reps, count     'Algorithm variables                    
  DIM char0 AS STRING * 1              'Byte                               
  DIM char1 AS STRING * 1              'Byte
  DIM AS INTEGER xmin, xmax            'Image size
  DIM AS INTEGER ymin, ymax            'Image size
  DIM AS INTEGER px, py                'Pixel Coodinates
  DIM AS PReg pal(256)                 'Palette
  DIM AS LONG index                    'Buffer index
  DIM AS UINTEGER pxcol                 'Pixel color
  
  'Error handling
  ON LOCAL ERROR GOTO errhandler
  
  'Open file             
  file = FREEFILE
  OPEN filename FOR BINARY AS #file
                                                                           
  'Calculate image size
  SEEK #file, 5
  GET #file,,char0:GET #file,,char1:xmin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:xmax=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymax=ASC(char0) + 256 * ASC(char1)
  ax = xmax - xmin + 1
  ay = ymax - ymin + 1
  Size = ax * ay
  
  'Read palette                                                  
  SEEK #file, LOF(file) - 767
  FOR i = 0 TO 255
    GET #file, , char0: pal(i).r = ASC(char0)
    GET #file, , char0: pal(i).g = ASC(char0)
    GET #file, , char0: pal(i).b = ASC(char0)
  NEXT i
  
  'Decoding loop                                                 
  px=0:py=0:index=0
  SEEK #file, 129
  DO WHILE count < Size
    GET #file, , char0
    asci = ASC(char0)
    IF asci >= 192 THEN
      reps = asci AND &H3F
      GET #file, , char0
    ELSE
      reps = 1
    ENDIF
    FOR l = 0 TO reps - 1
      IF trans <> -1 AND ASC(char0) = trans THEN
        pxcol = TRANCOLHI
      ELSE
        pxcol = RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
      ENDIF
      IF px < xlim AND py < ylim THEN
        img(index) = pxcol: index = index + 1
      ENDIF
      px = px + 1
      IF px = ax THEN: px = 0: py = py + 1: ENDIF
    NEXT l
    count = count + reps
  LOOP

  'Check file size limit
  IF ax > xlim OR ay > xlim THEN
    IF ax > xlim THEN ax = xlim
    IF ay > ylim THEN ay = ylim
  ENDIF

  'Close file & exit
  CLOSE #file
  EXIT SUB
  
  'Error handler
  errhandler:
  ax = 0
  ay = 0
  CLOSE #file
  EXIT SUB
  
END SUB

' Load PCX file and dump to screen (for High color modes)                               
SUB PcxDump (filename AS STRING, x AS INTEGER, y AS INTEGER)
                                                                            
  'Variables                                                     
  DIM AS INTEGER file
  DIM AS INTEGER i                 'Counter
  DIM AS INTEGER asci              'ASCII value
  DIM AS LONG Size, l, reps, count 'Algorithm variables                    
  DIM char0 AS STRING * 1          'Byte                               
  DIM char1 AS STRING * 1          'Byte
  DIM AS INTEGER xmin, xmax        'Image size
  DIM AS INTEGER ymin, ymax        'Image size
  DIM AS PReg pal(256)             'Palette
  DIM AS INTEGER ax, ay            'Image size
  DIM AS INTEGER px, py            'Screen coordinates
  DIM AS INTEGER errcode           'Error code
  
  'Error handling
  ON LOCAL ERROR GOTO errhandler

  'Open file             
  file = FREEFILE
  errcode = OPEN( filename FOR BINARY AS #file)
  IF errcode <> 0 THEN
    EXIT SUB
  ENDIF
                                                                           
  'Read header
  'GET #file, , pcxhead
  
  'Calculate image size
  SEEK #file, 5
  GET #file,,char0:GET #file,,char1:xmin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymin=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:xmax=ASC(char0) + 256 * ASC(char1)
  GET #file,,char0:GET #file,,char1:ymax=ASC(char0) + 256 * ASC(char1)
  ax = xmax - xmin + 1
  ay = ymax - ymin + 1
  Size = ax * ay
  
  'Read palette                                                  
  SEEK #file, LOF(file) - 767
  FOR i = 0 TO 255
    GET #file, , char0: pal(i).r = ASC(char0)
    GET #file, , char0: pal(i).g = ASC(char0)
    GET #file, , char0: pal(i).b = ASC(char0)
  NEXT i%
                                                                           
  'Decoding loop                                                 
  px = x: py = y
  SEEK #file, 129
  DO WHILE count < Size
    GET #file, , char0
    asci = ASC(char0)
    IF asci >= 192 THEN
      reps = asci AND &H3F
      GET #file, , char0
      FOR l = 0 TO reps - 1
        PSET (px,py), RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
        px = px + 1
        if px = x + ax then 
          px = x
          py = py + 1
        endif
      NEXT l
      count = count + reps
    ELSE
      PSET (px,py), RGB(pal(ASC(char0)).r,pal(ASC(char0)).g,pal(ASC(char0)).b)
      px = px + 1
      if px = x + ax then 
        px = x
        py = py + 1
      endif
      count = count + 1
    END IF
  LOOP

  'Close file
  CLOSE #file
  EXIT SUB
  
  'Error handler
  errhandler:
  EXIT SUB
  
END SUB

' Set palette
SUB SetPalette (pal() AS PReg)

  ' Variables
  DIM AS INTEGER i

' Palette set loop
  OUT &H3C8, 0
  FOR i = 0 TO 255
     OUT &H3C9, pal(i).r
     OUT &H3C9, pal(i).g
     OUT &H3C9, pal(i).b
  NEXT i

END SUB

'Set background image
SUB SetBackground(fname AS STRING)
  
  'Variables
  REDIM AS UINTEGER img(AX0*AY0)
  DIM AS INTEGER x,y,ax,ay
  
  'Load image file
  PcxLoad fname,ax,ay,img(),AX0,AY0,-1
  IF ax = 0 OR ay = 0 THEN EXIT SUB
  
  'Print loop
  x = 0: y = 0
  DO
    ImgDisplay x,y,ax,ay,@img(0),0
    x = x + ax
    IF x >= AX0 THEN
      x = 0
      y = y + ay
      IF y >= AY0 THEN EXIT DO
    ENDIF
  LOOP

END SUB

