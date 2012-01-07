      PROGRAM TTIDBS
C     PROGRAM  TTIDBS(OUTPUT,TAPE6=OUTPUT)                              ID000070
C THIS PROGRAM IS A TEST PROGRAM FOR THE IDBVIP/IDSFFT SUBPRO-          ID000080
C GRAM PACKAGE.  ALL ELEMENTS OF RESULTING DZI1 AND DZI2 ARRAYS         ID000090
C ARE EXPECTED TO BE ZERO.                                              ID000100
C THE LUN CONSTANT IN THE LAST DATA INITIALIZATION STATEMENT IS         ID000110
C THE LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,           ID000120
C THEREFORE, SYSTEM DEPENDENT.                                          ID000130
C DECLARATION STATEMENTS                                                ID000140
      IMPLICIT DOUBLE PRECISION (A-D,P-Z)
      DIMENSION   XD(30),YD(30),ZD(30),                                 ID000150
     1            XI(6),YI(5),ZI(6,5),MISSI(6,5),                       ID000160
     2            ZI1(6,5),ZI2(6,5),DZI1(6,5),DZI2(6,5),                ID000170
     3            IWK(1030),WK(240)                                     ID000180
      LOGICAL MISSI
      DATA  NCP/4/                                                      ID000190
      DATA  NDP/30/                                                     ID000200
      DATA  XD(1), XD(2), XD(3), XD(4), XD(5), XD(6),                   ID000210
     1      XD(7), XD(8), XD(9), XD(10),XD(11),XD(12),                  ID000220
     2      XD(13),XD(14),XD(15),XD(16),XD(17),XD(18),                  ID000230
     3      XD(19),XD(20),XD(21),XD(22),XD(23),XD(24),                  ID000240
     4      XD(25),XD(26),XD(27),XD(28),XD(29),XD(30)/                  ID000250
     5      11.16, 24.20, 19.85, 10.35, 19.72,  0.00,                   ID000260
     6      20.87, 19.99, 10.28,  4.51,  0.00, 16.70,                   ID000270
     7       6.08, 25.00, 14.90,  0.00,  9.66,  5.22,                   ID000280
     8      11.77, 15.10, 25.00, 25.00, 14.59, 15.20,                   ID000290
     9       5.23,  2.14,  0.51, 25.00, 21.67,  3.31/                   ID000300
      DATA  YD(1), YD(2), YD(3), YD(4), YD(5), YD(6),                   ID000310
     1      YD(7), YD(8), YD(9), YD(10),YD(11),YD(12),                  ID000320
     2      YD(13),YD(14),YD(15),YD(16),YD(17),YD(18),                  ID000330
     3      YD(19),YD(20),YD(21),YD(22),YD(23),YD(24),                  ID000340
     4      YD(25),YD(26),YD(27),YD(28),YD(29),YD(30)/                  ID000350
     5       1.24, 16.23, 10.72,  4.11,  1.39, 20.00,                   ID000360
     6      20.00,  4.62, 15.16, 20.00,  4.48, 19.65,                   ID000370
     7       4.58, 11.87,  3.12,  0.00, 20.00, 14.66,                   ID000380
     8      10.47, 17.19,  3.87,  0.00,  8.71,  0.00,                   ID000390
     9      10.72, 15.03,  8.37, 20.00, 14.36,  0.13/                   ID000400
      DATA  ZD(1), ZD(2), ZD(3), ZD(4), ZD(5), ZD(6),                   ID000410
     1      ZD(7), ZD(8), ZD(9), ZD(10),ZD(11),ZD(12),                  ID000420
     2      ZD(13),ZD(14),ZD(15),ZD(16),ZD(17),ZD(18),                  ID000430
     3      ZD(19),ZD(20),ZD(21),ZD(22),ZD(23),ZD(24),                  ID000440
     4      ZD(25),ZD(26),ZD(27),ZD(28),ZD(29),ZD(30)/                  ID000450
     5      22.15,  2.83,  7.97, 22.33, 16.83, 34.60,                   ID000460
     6       5.74, 14.72, 21.59, 15.61, 61.77,  6.31,                   ID000470
     7      35.74,  4.40, 21.70, 58.20,  4.73, 40.36,                   ID000480
     8      13.62, 12.57,  8.74, 12.00, 14.81, 21.60,                   ID000490
     9      26.50, 53.10, 49.43,  0.60,  5.52, 44.08/                   ID000500
      DATA  NXI/6/, NYI/5/                                              ID000510
      DATA  XI(1), XI(2), XI(3), XI(4), XI(5), XI(6)/                   ID000520
     1       0.00,  5.00, 10.00, 15.00, 20.00, 25.00/                   ID000530
      DATA  YI(1), YI(2), YI(3), YI(4), YI(5)/                          ID000540
     1       0.00,  5.00, 10.00, 15.00, 20.00/                          ID000550
      DATA  ZI(1,1),ZI(2,1),ZI(3,1),ZI(4,1),ZI(5,1),ZI(6,1),            ID000560
     1      ZI(1,2),ZI(2,2),ZI(3,2),ZI(4,2),ZI(5,2),ZI(6,2),            ID000570
     2      ZI(1,3),ZI(2,3),ZI(3,3),ZI(4,3),ZI(5,3),ZI(6,3),            ID000580
     3      ZI(1,4),ZI(2,4),ZI(3,4),ZI(4,4),ZI(5,4),ZI(6,4),            ID000590
     4      ZI(1,5),ZI(2,5),ZI(3,5),ZI(4,5),ZI(5,5),ZI(6,5)/            ID000600
     5      58.20, 39.55, 26.90, 21.71, 17.68, 12.00,                   ID000610
     6      61.58, 39.39, 22.04, 21.29, 14.36,  8.04,                   ID000620
     7      59.18, 27.39, 16.78, 13.25,  8.59,  5.36,                   ID000630
     8      52.82, 40.27, 22.76, 16.61,  7.40,  2.88,                   ID000640
     9      34.60, 14.05,  4.12,  3.17,  6.31,  0.60/                   ID000650
      DATA  LUN/6/                                                      ID000660
C CALCULATION                                                           ID000670
   10 MD=1                                                              ID000680
      DO 12  IYI=1,NYI                                                  ID000690
        DO 11  IXI=1,NXI                                                ID000700
          IF(IXI.NE.1.OR.IYI.NE.1)  MD=2                                ID000710
          MISSI(IXI,IYI)=.FALSE.
          CALL IDBVIP(MD,NCP,NDP,XD,YD,ZD,1,XI(IXI),YI(IYI),            ID000720
     1                ZI1(IXI,IYI),IWK,WK,MISSI)                        ID000730
   11   CONTINUE                                                        ID000740
   12 CONTINUE                                                          ID000750
   15 CALL IDSFFT(1,NCP,NDP,XD,YD,ZD,NXI,NYI,XI,YI,ZI2,IWK,WK,MISSI)    ID000760
      DO 17  IYI=1,NYI                                                  ID000770
        DO 16  IXI=1,NXI                                                ID000780
          DZI1(IXI,IYI)=ABS(ZI1(IXI,IYI)-ZI(IXI,IYI))                   ID000790
          DZI2(IXI,IYI)=ABS(ZI2(IXI,IYI)-ZI(IXI,IYI))                   ID000800
   16   CONTINUE                                                        ID000810
   17 CONTINUE                                                          ID000820
C PRINTING OF INPUT DATA                                                ID000830
C       WRITE (LUN,2020)  NDP                                             ID000840
      DO 23  IDP=1,NDP                                                  ID000850
C       WRITE (LUN,2021)                         ID000860
C       WRITE (LUN,2022)  IDP,XD(IDP),YD(IDP),ZD(IDP)                   ID000870
   23 CONTINUE                                                          ID000880
C PRINTING OF OUTPUT RESULTS                                            ID000890
C       WRITE (LUN,2030)                                                  ID000900
C       WRITE (LUN,2031)  YI                                              ID000910
      DO 33  IXI=1,NXI                                                  ID000920
C       WRITE (LUN,2032)  XI(IXI),(ZI1(IXI,IYI),IYI=1,NYI)              ID000930
   33 CONTINUE                                                          ID000940
C       WRITE (LUN,2040)                                                  ID000950
C       WRITE (LUN,2031)  YI                                              ID000960
      DO 43  IXI=1,NXI                                                  ID000970
C       WRITE (LUN,2032)  XI(IXI),(DZI1(IXI,IYI),IYI=1,NYI)             ID000980
   43 CONTINUE                                                          ID000990
C       WRITE (LUN,2050)                                                  ID001000
C       WRITE (LUN,2031)  YI                                              ID001010
      DO 53  IXI=1,NXI                                                  ID001020
C       WRITE (LUN,2032)  XI(IXI),(ZI2(IXI,IYI),IYI=1,NYI)              ID001030
   53 CONTINUE                                                          ID001040
C       WRITE (LUN,2060)                                                  ID001050
C       WRITE (LUN,2031)  YI                                              ID001060
      DO 63  IXI=1,NXI                                                  ID001070
C       WRITE (LUN,2032)  XI(IXI),(DZI2(IXI,IYI),IYI=1,NYI)             ID001080
   63 CONTINUE                                                          ID001090
C      STOP                                                              ID001100
C FORMAT STATEMENTS                                                     ID001110
 2020 FORMAT(1H1,6HTTIDBS/////3X,10HINPUT DATA,8X,5HNDP =,I3///         ID001120
     1   30H      I      XD     YD     ZD /)                            ID001130
 2021 FORMAT(1X)                                                        ID001140
 2022 FORMAT(5X,I2,2X,3F7.2)                                            ID001150
 2030 FORMAT(1H1,6HTTIDBS/////3X,17HIDBVIP SUBROUTINE///                ID001160
     1   26X,10HZI1(XI,YI))                                             ID001170
 2031 FORMAT(7X,2HXI,4X,3HYI=/12X,5F7.2/)                               ID001180
 2032 FORMAT(1X/1X,F9.2,2X,5F7.2)                                       ID001190
 2040 FORMAT(1X/////3X,10HDIFFERENCE///                                 ID001200
     1   25X,11HDZI1(XI,YI))                                            ID001210
 2050 FORMAT(1H1,6HTTIDBS/////3X,17HIDSFFT SUBROUTINE///                ID001220
     1   26X,10HZI2(XI,YI))                                             ID001230
 2060 FORMAT(1X/////3X,10HDIFFERENCE///                                 ID001240
     1   25X,11HDZI2(XI,YI))                                            ID001250
      END                                                               ID001260
