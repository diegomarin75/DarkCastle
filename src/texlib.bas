#INCLUDE "texlib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "conlib.bi"
#INCLUDE "vbcompat.bi"

'Texture memory control array
DIM SHARED TxPtr AS LONG = 0
DIM SHARED TxNum AS INTEGER = 0
REDIM SHARED TxRec(MAXTEXTUR) AS TxRecord
REDIM SHARED TextureMem(TEXTURMEM) AS UINTEGER
REDIM SHARED aTxRec(10) AS TxRecord
REDIM SHARED TxAux(MAXTEXTUR) AS TxRecord

'Double image buffer
REDIM SHARED imgpx1(TEXTURESZ) AS UINTEGER
REDIM SHARED imgpx2(TEXTURESZ*4) AS UINTEGER

'Texture loader configuration variables
DIM SHARED txl_factor AS INTEGER
DIM SHARED txl_texpath AS STRING

'Texture classification
REDIM SHARED TxCat(MAXTEXTUR) AS TexClasf 'Texture classification

' Get texture directory path
FUNCTION GetTexPath(txd AS INTEGER) AS STRING
  SELECT CASE txd 
    CASE 1: GetTexPath = "textures\wall\"      'Wall texture
    CASE 2: GetTexPath = "textures\ground\"    'Ground texture
    CASE 3: GetTexPath = "textures\object\"    'Object textures
    CASE 4: GetTexPath = "textures\entity\"    'Entity textures
  END SELECT
END FUNCTION

' Get texture category
FUNCTION GetTexCat(cat AS INTEGER) AS STRING
  SELECT CASE cat
    CASE 00: GetTexCat = "[all]"
    CASE 01: GetTexCat = "WOods"
    CASE 02: GetTexCat = "STone"
    CASE 03: GetTexCat = "MEtal"
    CASE 04: GetTexCat = "DOors"
    CASE 05: GetTexCat = "TRans"
    CASE 06: GetTexCat = "GLass"
    CASE 07: GetTexCat = "ARtis"
    CASE 08: GetTexCat = "DEvic"
    CASE 09: GetTexCat = "ORgan"
    CASE 10: GetTexCat = "WAter"
    CASE 11: GetTexCat = "OTher"
    CASE 99: GetTexCat = "[w/o]"
  END SELECT
END FUNCTION

'Texture classification
SUB TexClasif(commd AS STRING, txd AS INTEGER, texname AS STRING, _
              BYREF cname AS STRING, BYREF counter AS INTEGER)
  
  'Variables
  DIM AS INTEGER i,k, found
  DIM AS INTEGER file
  DIM AS INTEGER count
  STATIC AS INTEGER numclasf
  
  'Switch on command
  SELECT CASE commd
  
    'Read texture classification file
    CASE "READ"
     file = FREEFILE
     OPEN "texclasf.dat" FOR RANDOM as #file Len = SIZEOF(TexClasf)
     k=0
     WHILE NOT EOF(file) AND k < MAXTEXTUR
       GET #file,,TxCat(k)
       k = k + 1
     WEND
     CLOSE #file
     numclasf = k

    'Save texture clasification file
    CASE "WRITE"
      file = FREEFILE
      OPEN "texclasf.dat" FOR RANDOM as #file Len = SIZEOF(TexClasf)
      k=0
      WHILE k < numclasf
        PUT #file,,TxCat(k)
        k = k + 1
      WEND
      CLOSE #file
      
    'Get classification for texture
    CASE "GET"
      found = 0
      FOR i=0 TO numclasf - 1
        IF val(TxCat(i).txd) = txd AND TxCat(i).texname = texname THEN 
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        cname = TxCat(i).cname
      ELSE
        cname = ""
      ENDIF
      
    'Set classification for texture
    CASE "SET"
      found = 0
      FOR i=0 TO numclasf - 1
        IF val(TxCat(i).txd) = txd AND TxCat(i).texname = texname THEN 
          found = 1
          EXIT FOR
        ENDIF
      NEXT i
      IF found = 1 THEN
        TxCat(i).cname = cname
      ELSEIF found = 0 AND numclasf < MAXTEXTUR THEN
        i = numclasf
        TxCat(i).txd = STR$(txd)
        TxCat(i).texname = texname
        TxCat(i).cname = cname
        numclasf = numclasf + 1
      ENDIF
    
    'Count items in category
    CASE "COUNT"
      count = 0
      FOR i=0 TO numclasf - 1
        IF VAL(TxCat(i).txd) = txd AND TxCat(i).cname = cname THEN
          count = count + 1 
        ENDIF
      NEXT i
      counter = count
    
  END SELECT

END SUB

' Get texture memory occupied space
FUNCTION TexMemGetSpace AS STRING
  TexMemGetSpace = "Mem:" + FORMAT(TxNum / MAXTEXTUR,"000%") + " " + FORMAT(TxPtr / TEXTURMEM,"000%")
END FUNCTION

' Get texture memory occupied space
SUB TexMemGetOccupied(BYREF recused AS INTEGER, BYREF memused AS INTEGER)
  
  'Vartiables
  DIM AS SINGLE rec, mem
  
  'Return occupied memory
  rec = 100 * TxNum / MAXTEXTUR
  mem = 100 * TxPtr / TEXTURMEM
  recused = INT(rec)
  memused = INT(mem)
  
END SUB

'Texture loader configuration
SUB TexLoaderConfig(factor AS INTEGER, texpath AS STRING)
  txl_factor = factor
  txl_texpath = texpath
END SUB

' Load texture file with cache memory (once oly)
FUNCTION ReadTextureOnce(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR, buffer AS INTEGER) AS INTEGER

  'Variables
  DIM AS INTEGER RetCode
  
  'Check if texture was accessed just before
  IF TexDir = aTxRec(buffer).TexDir AND TexName = aTxRec(buffer).TexName THEN
    ax = aTxRec(buffer).Ax
    ay = aTxRec(buffer).Ay
    ReadTextureOnce = 0
    EXIT FUNCTION
  
  'Texture was not accessed just before: call normal function
  ELSE
    RetCode = ReadTexture( TexDir, TexName, ax, ay, ipt )
    IF RetCode = 0 THEN
      aTxRec(buffer).TexDir = TexDir
      aTxRec(buffer).TexName = TexName
      aTxRec(buffer).Ax = ax
      aTxRec(buffer).Ay = ay
      ReadTextureOnce = 0
    ELSE
      ReadTextureOnce = 1
    ENDIF
  ENDIF

END FUNCTION

' Read texture (pointer version)
FUNCTION ReadTexture(TexDir AS INTEGER, TexName AS STRING, BYREF ax AS INTEGER, BYREF ay AS INTEGER, BYREF ipt AS UINTEGER PTR) AS INTEGER

  'Variables
  DIM AS INTEGER i,j,found,index
  DIM AS INTEGER low,high,middle
  DIM AS INTEGER tax, tay, tax2
  DIM AS LONG start,length
  DIM AS PReg col0,col1,col2,col3,col4
  DIM AS STRING filename
  DIM AS STRING * 20 value
  DIM AS INTEGER numcol
  DIM AS TxRecord TxRec0
  
  'Search texture in cache memory (Binary search)
  IF BINSEARCH = 1 THEN
    IF txl_texpath = "" THEN 
      value = STR$(TexDir) + TexName
    ELSE
      value = STR$(0) + TexName
    ENDIF
    low = 0
    high = TxNum - 1
    found = 0
    WHILE low <= high
      middle = (low + high) / 2
      IF TxRec(middle).Sort > value THEN
        high = middle - 1
      ELSEIF TxRec(middle).Sort < value THEN
        low = middle + 1
      ELSE
        found = 1
        index = middle
        EXIT WHILE
      ENDIF
    WEND
  
  'Search texture in cache memory (Sequential search)
  ELSE
    found = 0
    FOR i = 0 TO TxNum - 1
      IF ( txl_texpath =  "" AND TxRec(i).TexName = TexName AND TxRec(i).TexDir = TexDir ) _
      OR ( txl_texpath <> "" AND TxRec(i).TexName = TexName AND TxRec(i).texpath = txl_texpath ) THEN
        found = 1
        index = i
        EXIT FOR
      ENDIF
    NEXT i
  ENDIF
  
  'Return image data from cache memory
  IF found = 1 THEN
    
    'Return data
    start = TxRec(index).start
    ipt = @TextureMem(start)
    ax = TxRec(index).ax
    ay = TxRec(index).ay
    
    'Return code
    ReadTexture = 0
   
  'Load new texture file & save to cache memory
  ELSE
    
    'Texture filename
    IF txl_texpath = "" THEN
      filename = GetTexPath(TexDir) + TexName
    ELSE
      filename = txl_texpath + TexName
    ENDIF

    'Load image data (normal images)
    IF ( txl_factor = 0 AND LCASE(RIGHT$(TexName,4)) = ".pcx" ) OR txl_factor = 1 THEN
      PcxLoad filename, tax, tay, imgpx1(), TEXTUREAX, TEXTUREAY, TRANCOL08
    
    'Load image data (double size images)
    ELSEIF ( txl_factor = 0 AND LCASE(RIGHT$(TexName,4)) = ".px2" ) OR txl_factor = 2 THEN
      PcxLoad filename, tax, tay, imgpx2(), TEXTUREAX*2, TEXTUREAY*2, TRANCOL08
      tax2 = tax / 2
      FOR i=0 TO tax - 1 STEP 2
      FOR j=0 TO tay - 1 STEP 2
        numcol = 0
        col1.r=0: col1.g=0: col1.b=0: col2.r=0: col2.g=0: col2.b=0
        col3.r=0: col3.g=0: col3.b=0: col4.r=0: col4.g=0: col4.b=0
        index = (i+0) + tax*(j+0)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col1.r=RGB_R(imgpx2(index)):col1.g=RGB_G(imgpx2(index)):col1.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+1) + tax*(j+0)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col2.r=RGB_R(imgpx2(index)):col2.g=RGB_G(imgpx2(index)):col2.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+1) + tax*(j+1)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col3.r=RGB_R(imgpx2(index)):col3.g=RGB_G(imgpx2(index)):col3.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        index = (i+0) + tax*(j+1)
        IF imgpx2(index) <> TRANCOLHI AND i <> tax - 1 AND j <> tay - 1 THEN
          col4.r=RGB_R(imgpx2(index)):col4.g=RGB_G(imgpx2(index)):col4.b=RGB_B(imgpx2(index))
          numcol = numcol + 1
        ENDIF
        IF numcol <> 0 THEN
          col0.r = (col1.r + col2.r + col3.r + col4.r) / numcol
          col0.g = (col1.g + col2.g + col3.g + col4.g) / numcol
          col0.b = (col1.b + col2.b + col3.b + col4.b) / numcol
          imgpx1((i/2)+tax2*(j/2)) = RGB(col0.r,col0.g,col0.b)
        ELSE
          imgpx1((i/2)+tax2*(j/2)) = TRANCOLHI
        ENDIF
      NEXT j
      NEXT i
      tax = tax / 2
      tay = tay / 2
    ENDIF
    
    'Return texture size
    ax = tax
    ay = tay
    
    'Check that there are free records to store the new image
    IF TxNum >= MAXTEXTUR THEN
      ReadTexture = 1
      EXIT FUNCTION
    ENDIF
    index = TxNum
    
    'Check if there is enough memory
    IF TxPtr+(tax*tay) >= TEXTURMEM THEN
      ReadTexture = 1
      EXIT FUNCTION
    ENDIF
    
    'Save image data
    start = TxPtr
    length = tax * tay
    FOR i = 0 TO length - 1
      TextureMem(start + i) = imgpx1(i)
    NEXT i
    
    'Return image pointer
    ipt = @TextureMem(start)
    
    'Save texture record as used
    IF txl_texpath = "" THEN
      TxRec0.TexDir = TexDir
      TxRec0.TexPath = ""
    ELSE
      TxRec0.TexDir = 0
      TxRec0.TexPath = txl_texpath
    ENDIF
    TxRec0.TexName = TexName
    TxRec0.Ax = tax
    TxRec0.Ay = tay
    TxRec0.Start = start
    TxRec0.Length = length
    TxRec0.Sort = STR$(TxRec0.TexDir) + TexName
    
    'Insert texture record
    InsertTexRecord(TxRec0)
    
    'Modify Memory pointer and texture counter
    TxNum = TxNum + 1
    TxPtr = TxPtr + length

    'Return code
    ReadTexture = 0

  ENDIF

END FUNCTION

'Insert texture record with sorting
SUB InsertTexRecord(TxRec0 AS TxRecord)

  'Variables
  DIM AS INTEGER i

  'Insert new record
  IF TxNum = 0 THEN
    TxRec(0) = TxRec0
  ELSE
    i = TxNum - 1
    DO
      IF i < 0 THEN EXIT DO
      IF TxRec(i).Sort <= TxRec0.Sort THEN EXIT DO
      TxRec(i+1) = TxRec(i)
      i = i - 1
    LOOP
    TxRec(i+1) = TxRec0
  ENDIF

END SUB

' Kill Texture memory
SUB KillTexMemory

  'Variables
  DIM AS INTEGER i
  
  'Initialize all records
  FOR i = 0 TO MAXTEXTUR - 1
    TxRec(i).TexDir = 0
    TxRec(i).TexPath = ""
    TxRec(i).TexName = ""
    TxRec(i).Ax = 0
    TxRec(i).Ay = 0
    TxRec(i).Start = 0
    TxRec(i).Length = 0
    TxRec(i).Sort = ""
  NEXT i
  
  'Initialize previous accessed textures
  FOR i=0 TO 9
    aTxRec(i).TexDir = 0
    aTxRec(i).TexPath = ""
    aTxRec(i).TexName = ""
    aTxRec(i).Ax = 0
    aTxRec(i).Ay = 0
    aTxRec(i).Start = 0
    aTxRec(i).Length = 0
    aTxRec(i).Sort = ""
  NEXT i
    
  'Reset texture counter and memory pointer
  TxNum = 0
  TxPtr = 0
  
END SUB