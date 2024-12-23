#INCLUDE "conlib.bi"
#INCLUDE "texlib.bi"
#INCLUDE "pcxlib.bi"
#INCLUDE "isolib.bi"

'Global variables (fonts)
DIM SHARED curfont AS INTEGER 'Current font
DIM SHARED fontax AS INTEGER  'Current font size x 
DIM SHARED fontay AS INTEGER  'Current font size y
DIM SHARED spacex AS INTEGER  'Current font spacing x
DIM SHARED spacey AS INTEGER  'Current font spacing y

'Global variables (pictograms)
DIM SHARED curpic AS INTEGER 'Current font
DIM SHARED picax AS INTEGER  'Current font size x 
DIM SHARED picay AS INTEGER  'Current font size y
DIM SHARED picspx AS INTEGER 'Current font spacing x
DIM SHARED picspy AS INTEGER 'Current font spacing y

'Global variables (PCX fonts)
DIM SHARED curpcx AS INTEGER    'Current font
DIM SHARED pcxax AS INTEGER     'Current font size x 
DIM SHARED pcxay AS INTEGER     'Current font size y
DIM SHARED pcxspx AS INTEGER    'Current font spacing x
DIM SHARED pcxspy AS INTEGER    'Current font spacing y
DIM SHARED pcxvsp AS INTEGER    'Spacing for variable space font
DIM SHARED pcxcenter AS INTEGER 'Center attribute

' ASCII character definition table
DIM SHARED asctable3x5(128) AS LONG
DIM SHARED asctable5x6(128) AS LONG

'Pictogram table
DIM SHARED pict11x15(8) AS pictogram11x15

'Keyboard buffer for manual input
DIM SHARED kbuffptr AS INTEGER = 0
DIM SHARED kbuffer(16) AS STRING * 2

'Get string exit mode
DIM SHARED gsexitmode AS INTEGER = GSEXIT_NORMAL
DIM SHARED gscurpos   AS INTEGER = 0

'PCX fonts
REDIM SHARED pcxfont(MAXPCXFN) AS pcxfdef
DIM SHARED modcol AS UINTEGER = 0

'ASCII table initialization
SUB InitFonts

  '3x5 font definition
  asctable3x5(0) = 0       ' NUL
  asctable3x5(1) = 3960    ' SOH
  asctable3x5(2) = 3960    ' STX
  asctable3x5(3) = 3960    ' ETX
  asctable3x5(4) = 3960    ' EOT
  asctable3x5(5) = 3960    ' ENQ
  asctable3x5(6) = 3960    ' ACK
  asctable3x5(7) = 3960    ' BEL
  asctable3x5(8) = 3960    ' BS
  asctable3x5(9) = 3960    ' TAB
  asctable3x5(10) = 3960   ' LF
  asctable3x5(11) = 3960   ' VT
  asctable3x5(12) = 3960   ' FF
  asctable3x5(13) = 3960   ' CR
  asctable3x5(14) = 3960   ' SO
  asctable3x5(15) = 3960   ' SI
  asctable3x5(16) = 3960   ' DEL
  asctable3x5(17) = 3960   ' XON
  asctable3x5(18) = 3960   ' DC2
  asctable3x5(19) = 3960   ' XOFF
  asctable3x5(20) = 3960   ' DC4
  asctable3x5(21) = 3960   ' NAK
  asctable3x5(22) = 3960   ' SYN
  asctable3x5(23) = 3960   ' ETB
  asctable3x5(24) = 3960   ' CAN
  asctable3x5(25) = 3960   ' EM
  asctable3x5(26) = 3960   ' SUB
  asctable3x5(27) = 3960   ' ESC
  asctable3x5(28) = 3960   ' FS
  asctable3x5(29) = 3960   ' GS
  asctable3x5(30) = 3960   ' RS
  asctable3x5(31) = 3960   ' US
  asctable3x5(32) = 0      ' SP
  asctable3x5(33) = 8338   ' !
  asctable3x5(34) = 45     ' "
  asctable3x5(35) = 24573  ' #
  asctable3x5(36) = 32223  ' $
  asctable3x5(37) = 21157  ' %
  asctable3x5(38) = 27306  ' &
  asctable3x5(39) = 18     ' '
  asctable3x5(40) = 17556  ' (
  asctable3x5(41) = 5265   ' )
  asctable3x5(42) = 2728   ' *
  asctable3x5(43) = 1488   ' +
  asctable3x5(44) = 10240  ' ,
  asctable3x5(45) = 448    ' -
  asctable3x5(46) = 08192  ' .
  asctable3x5(47) = 4772   ' /
  asctable3x5(48) = 11114  ' 0
  asctable3x5(49) = 29850  ' 1
  asctable3x5(50) = 29671  ' 2
  asctable3x5(51) = 31207  ' 3
  asctable3x5(52) = 18925  ' 4
  asctable3x5(53) = 31183  ' 5
  asctable3x5(54) = 31695  ' 6
  asctable3x5(55) = 18727  ' 7
  asctable3x5(56) = 31727  ' 8
  asctable3x5(57) = 31215  ' 9
  asctable3x5(58) = 1040   ' :
  asctable3x5(59) = 9232   ' ;
  asctable3x5(60) = 17492  ' <
  asctable3x5(61) = 3640   ' =
  asctable3x5(62) = 5393   ' >
  asctable3x5(63) = 8359   ' ?
  asctable3x5(64) = 3960   ' @
  asctable3x5(65) = 24431  ' A
  asctable3x5(66) = 15083  ' B
  asctable3x5(67) = 29263  ' C
  asctable3x5(68) = 15211  ' D
  asctable3x5(69) = 29647  ' E
  asctable3x5(70) = 5071   ' F
  asctable3x5(71) = 31311  ' G
  asctable3x5(72) = 23533  ' H
  asctable3x5(73) = 9362   ' I
  asctable3x5(74) = 31524  ' J
  asctable3x5(75) = 23277  ' K
  asctable3x5(76) = 29257  ' L
  asctable3x5(77) = 23551  ' M
  asctable3x5(78) = 23407  ' N
  asctable3x5(79) = 31599  ' O
  asctable3x5(80) = 5103   ' P
  asctable3x5(81) = 18287  ' Q
  asctable3x5(82) = 23279  ' R
  asctable3x5(83) = 31183  ' S
  asctable3x5(84) = 9367   ' T
  asctable3x5(85) = 31597  ' U
  asctable3x5(86) = 11117  ' V
  asctable3x5(87) = 32621  ' W
  asctable3x5(88) = 23213  ' X
  asctable3x5(89) = 9709   ' Y
  asctable3x5(90) = 29351  ' Z
  asctable3x5(91) = 25750  ' [
  asctable3x5(92) = 17553  ' \
  asctable3x5(93) = 26918  ' ]
  asctable3x5(94) = 42     ' ^
  asctable3x5(95) = 28672  ' _
  asctable3x5(96) = 17     ' `
  asctable3x5(97) = 31168  ' a
  asctable3x5(98) = 31689  ' b
  asctable3x5(99) = 29632  ' c
  asctable3x5(100) = 31716 ' d
  asctable3x5(101) = 30656 ' e
  asctable3x5(102) = 9686  ' f
  asctable3x5(103) = 15744 ' g
  asctable3x5(104) = 23497 ' h
  asctable3x5(105) = 9346  ' i
  asctable3x5(106) = 31492 ' j
  asctable3x5(107) = 22344 ' k
  asctable3x5(108) = 25746 ' l
  asctable3x5(109) = 24512 ' m
  asctable3x5(110) = 23488 ' n
  asctable3x5(111) = 31680 ' o
  asctable3x5(112) = 8128  ' p
  asctable3x5(113) = 20416 ' q
  asctable3x5(114) = 5056  ' r
  asctable3x5(115) = 31104 ' s
  asctable3x5(116) = 25786 ' t
  asctable3x5(117) = 31552 ' u
  asctable3x5(118) = 11072 ' v
  asctable3x5(119) = 32576 ' w
  asctable3x5(120) = 21824 ' x
  asctable3x5(121) = 9536  ' y
  asctable3x5(122) = 29496 ' z
  asctable3x5(123) = 25814 ' {
  asctable3x5(124) = 9362  ' |
  asctable3x5(125) = 13715 ' }
  asctable3x5(126) = 408   ' ~
  asctable3x5(127) = 3960  ' 

  '5x6 Font definition
  asctable5x6(000) = 0000000000 ' NUL
  asctable5x6(001) = 0033081312 ' SOH
  asctable5x6(002) = 0033081312 ' STX
  asctable5x6(003) = 0033081312 ' ETX
  asctable5x6(004) = 0033081312 ' EOT
  asctable5x6(005) = 0033081312 ' ENQ
  asctable5x6(006) = 0033081312 ' ACK
  asctable5x6(007) = 0033081312 ' BEL
  asctable5x6(008) = 0033081312 ' BS
  asctable5x6(009) = 0033081312 ' TAB
  asctable5x6(010) = 0033081312 ' LF
  asctable5x6(011) = 0033081312 ' VT
  asctable5x6(012) = 0033081312 ' FF
  asctable5x6(013) = 0033081312 ' CR
  asctable5x6(014) = 0033081312 ' SO
  asctable5x6(015) = 0033081312 ' SI
  asctable5x6(016) = 0033081312 ' DEL
  asctable5x6(017) = 0033081312 ' XON
  asctable5x6(018) = 0033081312 ' DC2
  asctable5x6(019) = 0033081312 ' XOFF
  asctable5x6(020) = 0033081312 ' DC4
  asctable5x6(021) = 0033081312 ' NAK
  asctable5x6(022) = 0033081312 ' SYN
  asctable5x6(023) = 0033081312 ' ETB
  asctable5x6(024) = 0033081312 ' CAN
  asctable5x6(025) = 0033081312 ' EM
  asctable5x6(026) = 0033081312 ' SUB
  asctable5x6(027) = 0033081312 ' ESC
  asctable5x6(028) = 0033081312 ' FS
  asctable5x6(029) = 0033081312 ' GS
  asctable5x6(030) = 0033081312 ' RS
  asctable5x6(031) = 0033081312 ' US
  asctable5x6(032) = 0000000000 ' SP
  asctable5x6(033) = 0134353028 ' !
  asctable5x6(034) = 0000000330 ' "
  asctable5x6(035) = 0368389098 ' #
  asctable5x6(036) = 1062180031 ' $
  asctable5x6(037) = 0572657937 ' %
  asctable5x6(038) = 0748365094 ' &
  asctable5x6(039) = 0000000132 ' '
  asctable5x6(040) = 0272765064 ' (
  asctable5x6(041) = 0071438466 ' )
  asctable5x6(042) = 0921697755 ' *
  asctable5x6(043) = 0139432064 ' +
  asctable5x6(044) = 0138412032 ' ,
  asctable5x6(045) = 0001015808 ' -
  asctable5x6(046) = 0134217728 ' .
  asctable5x6(047) = 0035787024 ' /
  asctable5x6(048) = 0488162862 ' 0
  asctable5x6(049) = 0474091716 ' 1
  asctable5x6(050) = 1008869964 ' 2
  asctable5x6(051) = 0488534574 ' 3
  asctable5x6(052) = 0277850504 ' 4
  asctable5x6(053) = 0520553564 ' 5
  asctable5x6(054) = 0488223806 ' 6
  asctable5x6(055) = 0035791391 ' 7
  asctable5x6(056) = 0488064558 ' 8
  asctable5x6(057) = 0487540270 ' 9
  asctable5x6(058) = 0004194432 ' :
  asctable5x6(059) = 0138412160 ' ;
  asctable5x6(060) = 0136349824 ' <
  asctable5x6(061) = 0032537600 ' =
  asctable5x6(062) = 0143138944 ' >
  asctable5x6(063) = 0134361646 ' ?
  asctable5x6(064) = 1008391726 ' @
  asctable5x6(065) = 0589284927 ' A
  asctable5x6(066) = 0521651759 ' B
  asctable5x6(067) = 0487622190 ' C
  asctable5x6(068) = 0521717295 ' D
  asctable5x6(069) = 1042252863 ' E
  asctable5x6(070) = 0035619903 ' F
  asctable5x6(071) = 1058965055 ' G
  asctable5x6(072) = 0589284913 ' H
  asctable5x6(073) = 0474091662 ' I
  asctable5x6(074) = 1058554384 ' J
  asctable5x6(075) = 0307334313 ' K
  asctable5x6(076) = 1041269793 ' L
  asctable5x6(077) = 0588972027 ' M
  asctable5x6(078) = 0597481139 ' N
  asctable5x6(079) = 1058588223 ' O
  asctable5x6(080) = 0035112495 ' P
  asctable5x6(081) = 0552125998 ' Q
  asctable5x6(082) = 0307742255 ' R
  asctable5x6(083) = 0520554030 ' S
  asctable5x6(084) = 0138547359 ' T
  asctable5x6(085) = 0488162865 ' U
  asctable5x6(086) = 0145041969 ' V
  asctable5x6(087) = 0358274737 ' W
  asctable5x6(088) = 0581046609 ' X
  asctable5x6(089) = 0138547537 ' Y
  asctable5x6(090) = 1041441311 ' Z
  asctable5x6(091) = 0943853724 ' [
  asctable5x6(092) = 0545394753 ' \
  asctable5x6(093) = 0239210631 ' ]
  asctable5x6(094) = 0000017732 ' ^
  asctable5x6(095) = 1040187392 ' _
  asctable5x6(096) = 0000000130 ' `
  asctable5x6(097) = 0211230912 ' a
  asctable5x6(098) = 0244620320 ' b
  asctable5x6(099) = 0470844864 ' c
  asctable5x6(100) = 0479508736 ' d
  asctable5x6(101) = 0471311552 ' e
  asctable5x6(102) = 0069437824 ' f
  asctable5x6(103) = 0210183360 ' g
  asctable5x6(104) = 0311657504 ' h
  asctable5x6(105) = 0138543232 ' i
  asctable5x6(106) = 0104988800 ' j
  asctable5x6(107) = 0439526432 ' k
  asctable5x6(108) = 0237045856 ' l
  asctable5x6(109) = 0727373120 ' m
  asctable5x6(110) = 0311731392 ' n
  asctable5x6(111) = 0488162752 ' o
  asctable5x6(112) = 0034841824 ' p
  asctable5x6(113) = 0277292480 ' q
  asctable5x6(114) = 0034639264 ' r
  asctable5x6(115) = 0243467712 ' s
  asctable5x6(116) = 0203496512 ' t
  asctable5x6(117) = 0211068192 ' u
  asctable5x6(118) = 0206742816 ' v
  asctable5x6(119) = 0358270496 ' w
  asctable5x6(120) = 0308487456 ' x
  asctable5x6(121) = 0104999200 ' y
  asctable5x6(122) = 0505553376 ' z
  asctable5x6(123) = 0136416324 ' {
  asctable5x6(124) = 0138547332 ' |
  asctable5x6(125) = 0143417604 ' }
  asctable5x6(126) = 0000317440 ' ~
  asctable5x6(127) = 0015728064 '

END SUB

' Pictogram table initialization
SUB InitPictograms

  'Mode map
  pict11x15(PIC_MODE_MAP).row(00) = 0000
  pict11x15(PIC_MODE_MAP).row(01) = 0000
  pict11x15(PIC_MODE_MAP).row(02) = 0032
  pict11x15(PIC_MODE_MAP).row(03) = 0080
  pict11x15(PIC_MODE_MAP).row(04) = 0136
  pict11x15(PIC_MODE_MAP).row(05) = 0260
  pict11x15(PIC_MODE_MAP).row(06) = 0514
  pict11x15(PIC_MODE_MAP).row(07) = 1025
  pict11x15(PIC_MODE_MAP).row(08) = 0514
  pict11x15(PIC_MODE_MAP).row(09) = 0260
  pict11x15(PIC_MODE_MAP).row(10) = 0136
  pict11x15(PIC_MODE_MAP).row(11) = 0080
  pict11x15(PIC_MODE_MAP).row(12) = 0032
  pict11x15(PIC_MODE_MAP).row(13) = 0000
  pict11x15(PIC_MODE_MAP).row(14) = 0000
  
  'Mode cells
  pict11x15(PIC_MODE_CELL).row(00) = 0016
  pict11x15(PIC_MODE_CELL).row(01) = 0040
  pict11x15(PIC_MODE_CELL).row(02) = 0068
  pict11x15(PIC_MODE_CELL).row(03) = 0108
  pict11x15(PIC_MODE_CELL).row(04) = 0340
  pict11x15(PIC_MODE_CELL).row(05) = 0724
  pict11x15(PIC_MODE_CELL).row(06) = 1108
  pict11x15(PIC_MODE_CELL).row(07) = 1748
  pict11x15(PIC_MODE_CELL).row(08) = 1370
  pict11x15(PIC_MODE_CELL).row(09) = 1361
  pict11x15(PIC_MODE_CELL).row(10) = 1371
  pict11x15(PIC_MODE_CELL).row(11) = 1365
  pict11x15(PIC_MODE_CELL).row(12) = 0949
  pict11x15(PIC_MODE_CELL).row(13) = 0286
  pict11x15(PIC_MODE_CELL).row(14) = 0004
  
  'Mode attributes
  pict11x15(PIC_MODE_ATTR).row(00) = 0000
  pict11x15(PIC_MODE_ATTR).row(01) = 0066
  pict11x15(PIC_MODE_ATTR).row(02) = 0036
  pict11x15(PIC_MODE_ATTR).row(03) = 0024
  pict11x15(PIC_MODE_ATTR).row(04) = 0024
  pict11x15(PIC_MODE_ATTR).row(05) = 0036
  pict11x15(PIC_MODE_ATTR).row(06) = 0066
  pict11x15(PIC_MODE_ATTR).row(07) = 0000
  pict11x15(PIC_MODE_ATTR).row(08) = 0256
  pict11x15(PIC_MODE_ATTR).row(09) = 0128
  pict11x15(PIC_MODE_ATTR).row(10) = 0064
  pict11x15(PIC_MODE_ATTR).row(11) = 0034
  pict11x15(PIC_MODE_ATTR).row(12) = 0020
  pict11x15(PIC_MODE_ATTR).row(13) = 0008
  pict11x15(PIC_MODE_ATTR).row(14) = 0000
  
  'Mode overlay textures
  pict11x15(PIC_MODE_OVL).row(00) = 0255
  pict11x15(PIC_MODE_OVL).row(01) = 0129
  pict11x15(PIC_MODE_OVL).row(02) = 0129
  pict11x15(PIC_MODE_OVL).row(03) = 0129
  pict11x15(PIC_MODE_OVL).row(04) = 2041
  pict11x15(PIC_MODE_OVL).row(05) = 1033
  pict11x15(PIC_MODE_OVL).row(06) = 1033
  pict11x15(PIC_MODE_OVL).row(07) = 1033
  pict11x15(PIC_MODE_OVL).row(08) = 1033
  pict11x15(PIC_MODE_OVL).row(09) = 1033
  pict11x15(PIC_MODE_OVL).row(10) = 1039
  pict11x15(PIC_MODE_OVL).row(11) = 1032
  pict11x15(PIC_MODE_OVL).row(12) = 1032
  pict11x15(PIC_MODE_OVL).row(13) = 1032
  pict11x15(PIC_MODE_OVL).row(14) = 2040
  
  'Mode lights
  pict11x15(PIC_MODE_LIGHT).row(00) = 0514
  pict11x15(PIC_MODE_LIGHT).row(01) = 0032
  pict11x15(PIC_MODE_LIGHT).row(02) = 1137
  pict11x15(PIC_MODE_LIGHT).row(03) = 0032
  pict11x15(PIC_MODE_LIGHT).row(04) = 0514
  pict11x15(PIC_MODE_LIGHT).row(05) = 0112
  pict11x15(PIC_MODE_LIGHT).row(06) = 0112
  pict11x15(PIC_MODE_LIGHT).row(07) = 0112
  pict11x15(PIC_MODE_LIGHT).row(08) = 0112
  pict11x15(PIC_MODE_LIGHT).row(09) = 0112
  pict11x15(PIC_MODE_LIGHT).row(10) = 0112
  pict11x15(PIC_MODE_LIGHT).row(11) = 0112
  pict11x15(PIC_MODE_LIGHT).row(12) = 1651
  pict11x15(PIC_MODE_LIGHT).row(13) = 2047
  pict11x15(PIC_MODE_LIGHT).row(14) = 1020
  
  'Mode navigation
  pict11x15(PIC_MODE_NAV).row(00) = 0063
  pict11x15(PIC_MODE_NAV).row(01) = 0063
  pict11x15(PIC_MODE_NAV).row(02) = 0063
  pict11x15(PIC_MODE_NAV).row(03) = 0063
  pict11x15(PIC_MODE_NAV).row(04) = 0063
  pict11x15(PIC_MODE_NAV).row(05) = 0018
  pict11x15(PIC_MODE_NAV).row(06) = 0018
  pict11x15(PIC_MODE_NAV).row(07) = 0018
  pict11x15(PIC_MODE_NAV).row(08) = 0018
  pict11x15(PIC_MODE_NAV).row(09) = 0018
  pict11x15(PIC_MODE_NAV).row(10) = 0226
  pict11x15(PIC_MODE_NAV).row(11) = 0914
  pict11x15(PIC_MODE_NAV).row(12) = 1090
  pict11x15(PIC_MODE_NAV).row(13) = 1026
  pict11x15(PIC_MODE_NAV).row(14) = 1020
  
  'Mode sound
  pict11x15(PIC_MODE_SND).row(00) = 0000
  pict11x15(PIC_MODE_SND).row(01) = 0000
  pict11x15(PIC_MODE_SND).row(02) = 0000
  pict11x15(PIC_MODE_SND).row(03) = 1536
  pict11x15(PIC_MODE_SND).row(04) = 1792
  pict11x15(PIC_MODE_SND).row(05) = 2047
  pict11x15(PIC_MODE_SND).row(06) = 2047
  pict11x15(PIC_MODE_SND).row(07) = 2047
  pict11x15(PIC_MODE_SND).row(08) = 1864
  pict11x15(PIC_MODE_SND).row(09) = 1608
  pict11x15(PIC_MODE_SND).row(10) = 0048
  pict11x15(PIC_MODE_SND).row(11) = 0000
  pict11x15(PIC_MODE_SND).row(12) = 0000
  pict11x15(PIC_MODE_SND).row(13) = 0000
  pict11x15(PIC_MODE_SND).row(14) = 0000

  'Mode events
  pict11x15(PIC_MODE_EVT).row(00) = 0112
  pict11x15(PIC_MODE_EVT).row(01) = 0508
  pict11x15(PIC_MODE_EVT).row(02) = 0546 
  pict11x15(PIC_MODE_EVT).row(03) = 1161
  pict11x15(PIC_MODE_EVT).row(04) = 1057
  pict11x15(PIC_MODE_EVT).row(05) = 1317
  pict11x15(PIC_MODE_EVT).row(06) = 1057
  pict11x15(PIC_MODE_EVT).row(07) = 1763
  pict11x15(PIC_MODE_EVT).row(08) = 1025
  pict11x15(PIC_MODE_EVT).row(09) = 1285
  pict11x15(PIC_MODE_EVT).row(10) = 1025
  pict11x15(PIC_MODE_EVT).row(11) = 1161
  pict11x15(PIC_MODE_EVT).row(12) = 0546
  pict11x15(PIC_MODE_EVT).row(13) = 0508
  pict11x15(PIC_MODE_EVT).row(14) = 1022

  'Mode link points
  pict11x15(PIC_MODE_LNK).row(00) = 0000
  pict11x15(PIC_MODE_LNK).row(01) = 0000
  pict11x15(PIC_MODE_LNK).row(02) = 0062
  pict11x15(PIC_MODE_LNK).row(03) = 0065
  pict11x15(PIC_MODE_LNK).row(04) = 0065
  pict11x15(PIC_MODE_LNK).row(05) = 0321
  pict11x15(PIC_MODE_LNK).row(06) = 0833
  pict11x15(PIC_MODE_LNK).row(07) = 2033
  pict11x15(PIC_MODE_LNK).row(08) = 0833
  pict11x15(PIC_MODE_LNK).row(09) = 0321
  pict11x15(PIC_MODE_LNK).row(10) = 0065
  pict11x15(PIC_MODE_LNK).row(11) = 0065
  pict11x15(PIC_MODE_LNK).row(12) = 0062
  pict11x15(PIC_MODE_LNK).row(13) = 0000
  pict11x15(PIC_MODE_LNK).row(14) = 0000

END SUB

' PCX font initialization
SUB InitPcxFonts
  PCXFLoad("Verdana40x31.pcx",40,31,33,94,0)
  PCXFLoad("Verdana24x18.pcx",24,18,33,94,1)
  pcxcenter = 0
END SUB

'Set current font
SUB SetFont(font AS INTEGER)
  curfont = font
  SELECT CASE font
    CASE 1
      fontax = 3
      fontay = 5
      spacex = 1
      spacey = 1
    CASE 2
      fontax = 5
      fontay = 6
      spacex = 1
      spacey = 3
  END SELECT
END SUB

'Set current font
SUB SetPictogramFont(pic AS INTEGER)
  curpic = pic
  SELECT CASE pic
    CASE 1
      picax = 11
      picay = 15
      picspx = 1
      picspy = 3
  END SELECT
END SUB

'Set current PCX font
SUB SetPCXFont(pcx AS INTEGER)
  curpcx = pcx
  SELECT CASE pcx
    CASE 1
      pcxax = 20
      pcxay = 31
      pcxspx = 1
      pcxspy = 1
      pcxvsp = 5
    CASE 2
      pcxax = 12
      pcxay = 19
      pcxspx = 1
      pcxspy = 1
      pcxvsp = 3
  END SELECT
END SUB

' Print character function
SUB PrintChr (x AS INTEGER, y AS INTEGER, char as STRING, col AS UINTEGER)

  'Variables
  DIM AS INTEGER i, j
  DIM AS LONG value, count
  
  'Get char value
  SELECT CASE curfont
    CASE 1
      value = asctable3x5(ASC(MID$(char, 1, 1)))
    CASE 2
      value = asctable5x6(ASC(MID$(char, 1, 1)))
  END SELECT
  
  'Print loop
  count = 1
  FOR j = 0 TO fontay - 1
  FOR i = 0 TO fontax - 1
    IF (value AND count) <> 0 THEN
  	  PSET (x + i, y + j), col
    END IF
    count = count * 2
  NEXT i
  NEXT j

END SUB

' Print string function
SUB PrintStr (x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  
  'Get String length
  length = LEN(strn)
  
  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print string function with background
SUB PrintStrBkg (x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, cur AS INTEGER, bkg AS INTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  REDIM AS BYTE charbkg(16384)
  
  'Get String length
  length = LEN(strn)
  
  'Copy background image
  SCREENSET bkg
  GET (x,y)-STEP(((fontax + spacex)*length-spacex), fontay), charbkg
  SCREENSET cur
  PUT (x,y), charbkg, PSET

  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print string function with background color
SUB PrintStrBCol(x AS INTEGER, y AS INTEGER, strn AS STRING, col AS UINTEGER, bcol AS UINTEGER)

  'Variables
  DIM AS INTEGER xc, yc
  DIM AS INTEGER i
  DIM AS INTEGER length
  
  'Get String length
  length = LEN(strn)
  
  'Set background color
  LINE (x,y)-STEP(((fontax + spacex)*length)-spacex, fontay),bcol,BF

  'Print loop
  xc = x: yc = y
  FOR i = 1 TO length
    PrintChr xc, yc, MID$(strn, i, 1), col
    xc = xc + fontax + spacex
  NEXT i
  
END SUB

' Print pictogram
SUB PrintPictogram(x AS INTEGER, y AS INTEGER, pic AS INTEGER, col AS UINTEGER)

  'Variables
  DIM AS INTEGER i, j
  DIM AS USHORT value, count
  
  'Print loop
  FOR j = 0 TO picay - 1
    count = 1
    value = pict11x15(pic).row(j)
    FOR i = 0 TO picax - 1
      IF (value AND count) <> 0 THEN
    	  PSET (x + i, y + j), col
      END IF
      count = count * 2
    NEXT i
  NEXT j

END SUB

'Cursor functions for fonts
FUNCTION FCol(col AS INTEGER) AS INTEGER
  FCol = 2 + col * ( fontax + spacex )
END FUNCTION
FUNCTION FRow(row AS INTEGER) AS INTEGER
  FRow = 2 + row * ( fontay + spacey )
END FUNCTION  

'Cursor functions for pictograms
FUNCTION PCol(col AS INTEGER) AS INTEGER
  PCol = 2 + col * ( picax + picspx )
END FUNCTION
FUNCTION PRow(row AS INTEGER) AS INTEGER
  PRow = 2 + row * ( picay + picspy )
END FUNCTION  

'Cursor functions for PCX fonts
FUNCTION XCol(col AS INTEGER) AS INTEGER
  XCol = 2 + col * ( pcxax + pcxspx )
END FUNCTION
FUNCTION XRow(row AS INTEGER) AS INTEGER
  XRow = 2 + row * ( pcxay + pcxspy )
END FUNCTION  

'Manual input in keyboard buffer
SUB SetKeyb(keyb AS STRING)
  IF kbuffptr < 16 THEN
    kbuffer(kbuffptr) = keyb
    kbuffptr = kbuffptr + 1
  ENDIF
END SUB  

'Get keyboard key
FUNCTION GetKeyb(waitmode AS INTEGER) AS INTEGER

  'Variables
  DIM AS STRING keyb
  DIM AS INTEGER i,code1, code2, code
  DIM AS INTEGER count

  'Get key from manual input buffer
  IF kbuffptr > 0 THEN
    keyb = kbuffer(0)
    FOR i=1 TO kbuffptr-1
      kbuffer(i-1) = kbuffer(i)
    NEXT i
    kbuffptr = kbuffptr - 1
  
  'Get key and wait
  ELSEIF waitmode = 1 THEN
    count = 0
    DO
      SLEEP 10 
      keyb = INKEY$ 
      count = count + 1
    LOOP WHILE keyb = ""
  
  'Get key without waiting
  ELSEIF waitmode = 0 THEN
    keyb = INKEY$
    IF keyb = "" THEN
      GetKeyb = 0
      EXIT FUNCTION
    ENDIF
  ENDIF
  
  'Return key
  code1 = ASC(LEFT$(keyb, 1)): code2 = ASC(RIGHT$(keyb, 1))
  IF code1 = 255 THEN 
    GetKeyb = -code2
  ELSE
    GetKeyb = code1
  END IF	  

END FUNCTION

'Get string exit mode
SUB GetStringExitMode(mode AS INTEGER)
  gsexitmode = mode
END SUB

'Get string from keyboard with echo to the screen
FUNCTION GetStringBCol(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, _
                      bcol AS UINTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
  GetStringBCol = GetString(col,row,length,colr,bcol,0,1,strinit,ecode)
END FUNCTION

'Get string from keyboard with echo to the screen
FUNCTION GetStringBkg(col AS INTEGER, row AS INTEGER, length AS INTEGER, colr AS UINTEGER, _
                      bkg AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING
  GetStringBkg = GetString(col,row,length,colr,0,bkg,2,strinit,ecode)
END FUNCTION

'Get string from keyboard with echo to the screen
FUNCTION GetString(col AS INTEGER, row AS INTEGER, length AS INTEGER, _
                   colr AS UINTEGER, bcol AS UINTEGER, bkg AS INTEGER, _
                   mode AS INTEGER, strinit AS STRING, BYREF ecode AS INTEGER) AS STRING

  'Variables
  DIM AS INTEGER i,curs,code,char,blink 
  DIM AS INTEGER finish,redraw
  DIM AS STRING str0, str1

  'Loop
  str0 = strinit
  SELECT CASE gsexitmode
    CASE GSEXIT_NORMAL: curs = LEN(str0)
    CASE GSEXIT_UPDOWN: curs = gscurpos
  END SELECT
  IF curs > LEN(str0) THEN curs = LEN(str0)
  redraw = 1
  DO
    
    'Redraw string
    IF redraw = 1 THEN
      IF mode = 1 THEN
        PrintStrBCol FCol(col),FRow(row),SPACE(length),colr,bcol
      ELSEIF mode = 2 THEN
        PrintStrBkg FCol(col),FRow(row),SPACE(length),colr,0,bkg
      ENDIF
      PrintStr FCol(col),FRow(row),str0,colr
      IF curs > LEN(str0)-1 THEN
        char = ASC(" ")
      ELSE
        char = str0[curs]
      ENDIF
      redraw = 1
    ENDIF

    'Cursor blinking loop
    blink = 1
    DO
      IF mode = 1 THEN
        PrintStrBCol FCol(col+curs),FRow(row),SPACE(1),colr,bcol
      ELSEIF mode = 2 THEN
        PrintStrBkg FCol(col+curs),FRow(row),SPACE(1),colr,0,bkg
      ENDIF
      PrintChr FCol(col+curs),FRow(row)+0,CHR$(char),colr
      IF blink = 1 THEN
        PrintChr FCol(col+curs),FRow(row)+0,"_",colr
        PrintChr FCol(col+curs),FRow(row)+1,"_",colr
        blink = 0
      ELSE
        blink = 1
      ENDIF
      code = GetKeyb(0)
      IF code <> 0 THEN EXIT DO
      SLEEP 250
    LOOP

    'Switch on key
    SELECT CASE code
    
      CASE 8 'Return -> delete character
        IF curs > 0 THEN
          str1 = ""
          FOR i=0 TO LEN(str0)-1
            IF i <> curs-1 THEN 
              str1 = str1 + MID$(str0,i+1,1)
            ENDIF
          NEXT i
          str0 = str1
          curs = curs - 1
          redraw = 1
        ENDIF
        
      CASE -83 'Delete -> delete character
        IF curs >= 0 AND curs < LEN(str0) THEN
          str1 = ""
          FOR i=0 TO LEN(str0)-1
            IF i <> curs THEN 
              str1 = str1 + MID$(str0,i+1,1)
            ENDIF
          NEXT i
          IF LEN(str1) < LEN(str0) THEN
            str0 = str1
            redraw = 1
          ENDIF
        ENDIF
        
     CASE -71 'Home -> Move cursor
       curs = 0
       redraw = 1
        
     CASE -79 'End -> Move cursor
       curs = LEN(str0)
       redraw = 1
        
     CASE -75 'Left -> Move cursor
       curs = curs - 1
       IF curs < 0 THEN curs = 0
       gscurpos = curs
       redraw = 1
        
     CASE -77 'Right -> Move cursor
       curs = curs + 1
       IF curs > LEN(str0) THEN curs = LEN(str0)
       gscurpos = curs
       redraw = 1
 
     CASE 13 'Enter -> Finish string input
        GetString = str0
        finish = 1
      
      CASE 27 'Escape -> return empty string
        GetString = ""
        finish = 1
      
      CASE -72 'Cursor up -> exit
        IF gsexitmode = GSEXIT_UPDOWN THEN
          GetString = str0
          finish = 1
        ENDIF
      
      CASE -80 'Cursor down -> exit
        IF gsexitmode = GSEXIT_UPDOWN THEN
          GetString = str0
          finish = 1
        ENDIF
      
      CASE ELSE 'Rest of characters -> copy to string
        IF code >=32 AND code <=126 AND LEN(str0)+1 < length THEN
          str1 = ""
          IF curs <= LEN(str0)-1 THEN
            FOR i=0 TO LEN(str0)-1
              IF i = curs THEN 
                str1 = str1 + CHR$(code)
              ENDIF
              str1 = str1 + MID$(str0,i+1,1)
            NEXT i
          ELSE
            str1 = str0 + CHR$(code)
          ENDIF
          str0 = str1
          curs = curs + 1
          redraw = 1
        ENDIF
    
    END SELECT
    
  LOOP WHILE finish = 0
  
  'Exit code
  ecode = code
  
END FUNCTION

' Load PCX font
SUB PCXFLoad(filename AS STRING, ax AS INTEGER, ay AS INTEGER, _
             start AS INTEGER, num AS INTEGER, handle AS INTEGER)
  'Variables
  DIM AS INTEGER i,j,k,idx,found
  DIM AS INTEGER fax,fay
  DIM AS UINTEGER PTR ipt
    
  'Load font from cache memory
  PcxLoad(filename,fax,fay,pcxfont(handle).img(),ax,ay*num,0)
    
  'Set char map pointers
  FOR k=0 TO 255
    IF k >= start AND k <= start + num THEN
      pcxfont(handle).char(k) = @pcxfont(handle).img((k-start)*ax*ay)
    ELSE
      pcxfont(handle).char(k) = 0 
    ENDIF
  NEXT k    
  
  'Calculate minimum and maximum for chars
  FOR k=0 TO 255
    IF k >= start AND k <= start + num THEN
      pcxfont(handle).chmin(k) = ax
      pcxfont(handle).chmax(k) = 0
      FOR i = 0 TO ax - 1
      FOR j = 0 TO ay - 1
        ipt = pcxfont(handle).char(k)
        IF ipt[(ax * j) + i] <> TRANCOLHI THEN
          IF i < pcxfont(handle).chmin(k) THEN pcxfont(handle).chmin(k) = i
          IF i > pcxfont(handle).chmax(k) THEN pcxfont(handle).chmax(k) = i
        ENDIF
      NEXT j
      NEXT i
    ENDIF
  NEXT k
  
  
  'Set rest of font table fields
  pcxfont(handle).ax = ax
  pcxfont(handle).ay = ay
    
END SUB

'Put PCX font char
SUB PCXFSetColor(col AS UINTEGER)
  modcol = col
END SUB

'Put PCX font char
SUB PCXFCentered(center AS INTEGER)
  pcxcenter = center
END SUB

'Put PCX font char
SUB PCXFPrintChr(px AS INTEGER,py AS INTEGER,char AS INTEGER)
  IF pcxfont(curpcx-1).char(char) <> 0 THEN
    IF modcol <> 0 THEN
      ImgDisplayMod(px,py,pcxfont(curpcx-1).ax,pcxfont(curpcx-1).ay,pcxfont(curpcx-1).char(char),0,modcol)
    ELSE
      ImgDisplay(px,py,pcxfont(curpcx-1).ax,pcxfont(curpcx-1).ay,pcxfont(curpcx-1).char(char),0)
    ENDIF
  ENDIF
END SUB

'Put PCX font string in monospace mode
SUB PCXFPrintStrMS(px AS INTEGER, py AS INTEGER, strn AS STRING)
  
  'Variables
  DIM AS INTEGER i,position
    
  'Calculate position if centered attribute is set
  IF pcxcenter = 1 THEN
    position = (AX0 - (LEN(strn) * (pcxax+pcxspx)))/2
  ELSE
    position = px
  ENDIF
  
  'String loop
  FOR i=0 TO LEN(strn)-1
    PCXFPrintChr(position + i*(pcxax+pcxspx),py,strn[i])
  NEXT i

END SUB

'Put PCX font string in variable space mode
SUB PCXFPrintStrVS(px AS INTEGER, py AS INTEGER, strn AS STRING)
  
  'Variables
  DIM AS INTEGER i,position,length
    
  'Calculate position if centered attribute is set
  IF pcxcenter = 1 THEN
    length = 0
    FOR i=0 TO LEN(strn)-1
      length = length + pcxfont(curpcx-1).chmax(strn[i]) - pcxfont(curpcx-1).chmin(strn[i]) + pcxvsp
    NEXT i
    position = (AX0 - length)/2
  ELSE
    position = px - pcxfont(curpcx-1).chmin(strn[0])
  ENDIF
  
  'String loop
  FOR i=0 TO LEN(strn)-1
    PCXFPrintChr(position - pcxfont(curpcx-1).chmin(strn[i]),py,strn[i])
    IF pcxfont(curpcx-1).char(strn[i]) <> 0 THEN
      position = position + pcxfont(curpcx-1).chmax(strn[i]) - pcxfont(curpcx-1).chmin(strn[i]) + pcxvsp
    ELSE
      position = position + pcxvsp * 2
    ENDIF
  NEXT i
  
END SUB

