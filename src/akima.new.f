      SUBROUTINE SDBI3P(MD,NDP,XD,YD,ZD,NIP,XI,YI, ZI,IER, WK,IWK,
     +                  EXTRPI,NEAR,NEXT,DIST)
*
* Scattered-data bivariate interpolation
* (a master subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs bivariate interpolation when the data
* points are scattered in the x-y plane.  It is based on the
* revised Akima method that has the accuracy of a cubic (third-
* degree) polynomial.
*
* The input arguments are
*   MD  = mode of computation
*       = 1 for new XD-YD (default)
*       = 2 for old XD-YD, new ZD
*       = 3 for old XD-YD, old ZD,
*   NDP = number of data points (must be 10 or greater),
*   XD  = array of dimension NDP containing the x coordinates
*         of the data points,
*   YD  = array of dimension NDP containing the y coordinates
*         of the data points,
*   ZD  = array of dimension NDP containing the z values at
*         the data points,
*   NIP = number of output points at which interpolation is
*         to be performed (must be 1 or greater),
*   XI  = array of dimension NIP containing the x coordinates
*         of the output points,
*   YI  = array of dimension NIP containing the y coordinates
*         of the output points.
*
* The output arguments are
*   ZI  = array of dimension NIP, where interpolated z values
*         are to be stored,
*   IER = error flag
*       = 0 for no errors
*       = 1 for NDP = 9 or less
*       = 2 for NDP not equal to NDPPV
*       = 3 for NIP = 0 or less
*       = 9 for errors in SDTRAN called by this subroutine.
*
* The other arguments are
*   WK  = two-dimensional array of dimension NDP*17 used
*         internally as a work area,
*   IWK = two-dimensional integer array of dimension NDP*25
*         used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* The very first call to this subroutine and the call with a new
* NDP value or new XD and YD arrays must be made with MD=1.  The
* call with MD=2 must be preceded by another call with the same
* NDP value and same XD and YD arrays.  The call with MD=3 must
* be preceded by another call with the same NDP value and same
* XD, YD, and ZD arrays.  Between the call with MD=2 and its
* preceding call, the IWK array must not be disturbed.  Between
* the call with MD=3 and its preceding call, the WK and IWK
* arrays must not be disturbed.
*
* The user of this subroutine can save the storage, by NDP*6
* numerical storage units, by placing the statement
*     EQUIVALENCE (WK(1,1),IWK(1,20))
* in the program that calls this subroutine.
*
* The constant in the PARAMETER statement below is
*   NIPIMX = maximum number of output points to be processed
*            at a time.
* The constant value has been selected empirically.
*
* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NIPIMX
      PARAMETER        (NIPIMX=51)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IER,MD,NDP,NIP,NEAR(NDP),NEXT(NDP)

*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             WK(NDP,17),XD(NDP),XI(NIP),YD(NDP),
     +                 YI(NIP),ZD(NDP),ZI(NIP),DIST(NDP)
      INTEGER          IWK(NDP,25)
      LOGICAL          EXTRPI(NIP)
*     ..
*     .. Local Scalars ..
      INTEGER          IERT,IIP,NDPPV,NIPI,NL,NT
*     ..
*     .. Local Arrays ..
      INTEGER          ITLI(NIPIMX),KTLI(NIPIMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLCTN,SDPD3P,SDPLNL,SDTRAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
*     .. Save statement ..
      SAVE             NDPPV,NT,NL
*     ..
* Error check
      IF (NDP.LE.9) GO TO 20
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          NDPPV = NDP
      ELSE
          IF (NDP.NE.NDPPV) GO TO 30
      END IF
      IF (NIP.LE.0) GO TO 40
* Triangulates the x-y plane.  (for MD=1)
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          CALL SDTRAN(NDP,XD,YD, NT,IWK(1,1),NL,IWK(1,7),IERT,
     +                IWK(1,1),IWK(1,7),IWK(1,13),IWK(1,14),IWK(1,9),
     +                NEAR,NEXT,DIST)
*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT,
*    +                LIST,LPTR,LEND,LTRI,ITL)
          IF (IERT.GT.0) GO TO 50
      END IF
* Estimates partial derivatives at all data points.  (for MD=1,2)
      IF (MD.NE.3) THEN
          CALL SDPD3P(NDP,XD,YD,ZD, WK(1,1), WK(1,6),WK(1,15),WK(1,17),
     +                IWK(1,9),IWK(1,10),IWK(1,19))
*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
      END IF
* Locates all points at which interpolation is to be performed
* and interpolates the ZI values.  (for MD=1,2,3)
      DO 10 IIP = 1,NIP,NIPIMX
          NIPI = MIN(NIP-IIP+1,NIPIMX)
          CALL SDLCTN(NDP,XD,YD,NT,IWK(1,1),NL,IWK(1,7),NIPI,XI(IIP),
     +                YI(IIP), KTLI,ITLI)
*         CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
          CALL SDPLNL(NDP,XD,YD,ZD,NT,IWK(1,1),NL,IWK(1,7),WK(1,1),NIPI,
     +                XI(IIP),YI(IIP),KTLI,ITLI, ZI(IIP), EXTRPI(IIP))
*         CALL SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,
*    +                NIP,XI,YI,KTLI,ITLI, ZI)
   10 CONTINUE
* Normal return
      IER = 0
      RETURN
* Error exit
   20 WRITE (*,FMT=9000) MD,NDP
      IER = 1
      RETURN
   30 WRITE (*,FMT=9010) MD,NDP,NDPPV
      IER = 2
      RETURN
   40 WRITE (*,FMT=9020) MD,NDP,NIP
      IER = 3
      RETURN
   50 WRITE (*,FMT=9030)
      IER = 9
      RETURN
* Format statement for error message
 9000 FORMAT (' ',/,'*** SDBI3P Error 1: NDP = 9 or less',/,'    MD =',
     +       I5,',  NDP =',I5,/)
 9010 FORMAT (' ',/,'*** SDBI3P Error 2: NDP not equal to NDPPV',/,
     +       '    MD =',I5,',  NDP =',I5,',  NDPPV =',I5,/)
 9020 FORMAT (' ',/,'*** SDBI3P Error 3: NIP = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,',  NIP =',I5,/)
 9030 FORMAT ('    Error detected in SDTRAN called by SDBI3P',/)
      END


      SUBROUTINE SDSF3P(MD,NDP,XD,YD,ZD,NXI,XI,NYI,YI, ZI,IER, WK,IWK,
     +                  EXTRPI,NEAR,NEXT,DIST)
*
* Scattered-data smooth surface fitting
* (a master subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs smooth surface fitting when the data
* points are scattered in the x-y plane.  It is based on the
* revised Akima method that has the accuracy of a cubic (third-
* degree) polynomial.
*
* The input arguments are
*   MD  = mode of computation
*       = 1 for new XD-YD (default)
*       = 2 for old XD-YD, new ZD
*       = 3 for old XD-YD, old ZD,
*   NDP = number of data points (must be 10 or greater),
*   XD  = array of dimension NDP containing the x coordinates
*         of the data points,
*   YD  = array of dimension NDP containing the y coordinates
*         of the data points,
*   ZD  = array of dimension NDP containing the z values at
*         the data points,
*   NXI = number of output grid points in the x coordinate
*         (must be 1 or greater),
*   XI  = array of dimension NXI containing the x coordinates
*         of the output grid points,
*   NYI = number of output grid points in the y coordinate
*         (must be 1 or greater),
*   YI  = array of dimension NYI containing the y coordinates
*         of the output grid points.
*
* The output arguments are
*   ZI  = two-dimensional array of dimension NXI*NYI, where
*         the interpolated z values at the output grid points
*         are to be stored,
*   IER = error flag
*       = 0 for no errors
*       = 1 for NDP = 9 or less
*       = 2 for NDP not equal to NDPPV
*       = 3 for NXI = 0 or less
*       = 4 for NYI = 0 or less
*       = 9 for errors in SDTRAN called by this subroutine.
*
* The other arguments are
*   WK  = two-dimensional array of dimension NDP*36 used
*         internally as a work area,
*   IWK = two-dimensional integer array of dimension NDP*25
*         used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* The very first call to this subroutine and the call with a new
* NDP value or new XD and YD arrays must be made with MD=1.  The
* call with MD=2 must be preceded by another call with the same
* NDP value and same XD and YD arrays.  The call with MD=3 must
* be preceded by another call with the same NDP value and same
* XD, YD, and ZD arrays.  Between the call with MD=2 and its
* preceding call, the IWK array must not be disturbed.  Between
* the call with MD=3 and its preceding call, the WK and IWK
* arrays must not be disturbed.
*
* The user of this subroutine can save the storage, by NDP*6
* numeric storage units, by placing the statement
*     EQUIVALENCE (WK(1,1),IWK(1,20))
* in the program that calls this subroutine.
*
* The constant in the PARAMETER statement below is
*   NIPIMX = maximum number of output points to be processed
*            at a time.
* The constant value has been selected empirically.
*
* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NIPIMX
      PARAMETER        (NIPIMX=51)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IER,MD,NDP,NXI,NYI,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             WK(NDP,17),XD(NDP),XI(NXI),YD(NDP),
     +                 YI(NYI),ZD(NDP),ZI(NXI,NYI),DIST(NDP)
      INTEGER          IWK(NDP,25)
      LOGICAL          EXTRPI(NXI,NYI)
*     ..
*     .. Local Scalars ..
      INTEGER          IERT,IIP,IXI,IYI,NDPPV,NIPI,NL,NT
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             YII(NIPIMX)
      INTEGER          ITLI(NIPIMX),KTLI(NIPIMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLCTN,SDPD3P,SDPLNL,SDTRAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
*     .. Save statement ..
      SAVE             NDPPV,NT,NL
*     ..
* Error check
      IF (NDP.LE.9) GO TO 40
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          NDPPV = NDP
      ELSE
          IF (NDP.NE.NDPPV) GO TO 50
      END IF
      IF (NXI.LE.0) GO TO 60
      IF (NYI.LE.0) GO TO 70
* Triangulates the x-y plane.  (for MD=1)
      IF (MD.NE.2 .AND. MD.NE.3) THEN
          CALL SDTRAN(NDP,XD,YD, NT,IWK(1,1),NL,IWK(1,7),IERT,
     +                IWK(1,1),IWK(1,7),IWK(1,13),IWK(1,14),IWK(1,9),
     +                NEAR,NEXT,DIST)
*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT,
*    +                LIST,LPTR,LEND,LTRI,ITL)
          IF (IERT.GT.0) GO TO 80
      END IF
* Estimates partial derivatives at all data points.  (for MD=1,2)
      IF (MD.NE.3) THEN
          CALL SDPD3P(NDP,XD,YD,ZD, WK(1,1), WK(1,6),WK(1,15),WK(1,17),
     +                IWK(1,9),IWK(1,10),IWK(1,19))
*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
      END IF
* Locates all grid points at which interpolation is to be
* performed and interpolates the ZI values.  (for MD=1,2,3)
      DO 30 IYI = 1,NYI
          DO 10 IIP = 1,NIPIMX
              YII(IIP) = YI(IYI)
   10     CONTINUE
          DO 20 IXI = 1,NXI,NIPIMX
              NIPI = MIN(NXI-IXI+1,NIPIMX)
              CALL SDLCTN(NDP,XD,YD,NT,IWK(1,1),NL,IWK(1,7),NIPI,
     +                    XI(IXI),YII, KTLI,ITLI)
*             CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
              CALL SDPLNL(NDP,XD,YD,ZD,NT,IWK(1,1),NL,IWK(1,7),WK(1,1),
     +                    NIPI,XI(IXI),YII,KTLI,ITLI, ZI(IXI,IYI),
     +                    EXTRPI(IXI,IYI))
*             CALL SDPLNL(NDP,XD,YD,ZD,NT,ITP,NL,IPL,PDD,
*    +                    NIP,XI,YI,KTLI,ITLI, ZI)
   20     CONTINUE
   30 CONTINUE
* Normal return
      IER = 0
      RETURN
* Error exit
   40 WRITE (*,FMT=9000) MD,NDP
      IER = 1
      RETURN
   50 WRITE (*,FMT=9010) MD,NDP,NDPPV
      IER = 2
      RETURN
   60 WRITE (*,FMT=9020) MD,NDP,NXI,NYI
      IER = 3
      RETURN
   70 WRITE (*,FMT=9030) MD,NDP,NXI,NYI
      IER = 4
      RETURN
   80 WRITE (*,FMT=9040)
      IER = 9
      RETURN
* Format statement for error message
 9000 FORMAT (' ',/,'*** SDSF3P Error 1: NDP = 9 or less',/,'    MD =',
     +       I5,',  NDP =',I5,/)
 9010 FORMAT (' ',/,'*** SDSF3P Error 2: NDP not equal to NDPPV',/,
     +       '    MD =',I5,',  NDP =',I5,'  NDPPV =',I5,/)
 9020 FORMAT (' ',/,'*** SDSF3P Error 3: NXI = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,'  NXI =',I5,',  NYI =',I5,/)
 9030 FORMAT (' ',/,'*** SDSF3P Error 4: NYI = 0 or less',/,'    MD =',
     +       I5,',  NDP =',I5,'  NXI =',I5,',  NYI =',I5,/)
 9040 FORMAT ('    Error detected in SDTRAN called by SDSF3P',/)
      END


      SUBROUTINE SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT, LIST,LPTR,LEND,
     +                  LTRI,ITL,NEAR,NEXT,DIST)
*
* Triangulation of the data area in a plane with a scattered data
* point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area in the x-y plane with
* a scattered data point set.  It divides the data area into a
* number of triangles and determines line segments that form the
* border of the data area.
*
* This subroutine consists of the following two steps, i.e.,
* (1) basic triangulation in the convex hull of the data points,
* and (2) removal of thin triangles along the border line of the
* data area.  It calls the SDTRCH and SDTRTT subroutines, that
* correspond to Steps (1) and (2), respectively.
*
* The input arguments are
*   NDP  = number of data points (must be greater than 3),
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*
* The output arguments are
*   NT   = number of triangles (its maximum is 2*NDP-5),
*   IPT  = two-dimensional integer array of dimension
*          (3,NT), where the point numbers of the vertexes
*          of the ITth triangle are to be stored counter-
*          clockwise in the ITth column, where IT = 1, 2,
*          ..., NT,
*   NL   = number of border line segments (its maximum is
*          NDP),
*   IPL  = two-dimensional integer array of dimension
*          (2,NL), where the point numbers of the end
*          points of the (IL)th border line segment are to
*          be stored counterclockwise in the ILth column,
*          where IL = 1, 2, ..., NL, with the line segments
*          stored counterclockwise,
*   IERT = error flag
*        = 0 for no errors
*        = 1 for NDP = 3 or less
*        = 2 for identical data points
*        = 3 for all collinear data points.
*
* The other arguments are
*   LIST = integer array of dimension 6*NDP USED internally
*          as a work area,
*   LPTR = integer array of dimension 6*NDP USED internally
*          as a work area,
*   LEND = integer array of dimension NDP USED internally as
*          a work area,
*   LTRI = two-dimensional integer array of dimension 12*NDP
*          used internally as a work area.
*   ITL  = integer array of dimension NDP used internally as
*          a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          IERT,NDP,NL,NT,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP),DIST(NDP)
      INTEGER          IPL(2,*),IPT(3,*),ITL(NDP),LEND(NDP),LIST(6,NDP),
     +                 LPTR(6,NDP),LTRI(12,NDP)
*     ..
*     .. Local Scalars ..
      INTEGER          IERTL,IERTM,IP1
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDTRCH,SDTRTT
*     ..
* Basic triangulation
      CALL SDTRCH(NDP,XD,YD, NT,IPT,NL,IPL,IERTM,IERTL, LIST,LPTR,LEND,
     +            LTRI,NEAR,NEXT,DIST)
      IF (IERTM.NE.0) GO TO 10
      IF (IERTL.NE.0) GO TO 20
      IERT = 0
* Removal of thin triangles that share border line segments
      CALL SDTRTT(NDP,XD,YD, NT,IPT,NL,IPL, ITL)
      RETURN
* Error exit
   10 IF (IERTM.EQ.-1) THEN
          IERT = 1
          WRITE (*,FMT=9000) NDP
      ELSE IF (IERTM.EQ.-2) THEN
          IERT = 2
          WRITE (*,FMT=9010)
      ELSE
          IERT = 3
          IP1 = IERTM
          WRITE (*,FMT=9020) NDP,IP1,XD(IP1),YD(IP1)
      END IF
      RETURN
   20 IF (IERTL.EQ.1) THEN
          IERT = 4
          WRITE (*,FMT=9030) NDP
      ELSE IF (IERTL.EQ.2) THEN
          IERT = 5
          WRITE (*,FMT=9040)
      END IF
      RETURN
* Format statements
 9000 FORMAT (' ',/,'*** SDTRAN Error 1: NDP = 3 or less',/,'    NDP =',
     +       I5)
 9010 FORMAT (' ',/,'*** SDTRAN Error 2: ',
     +       'The first three data points are collinear.',/)
 9020 FORMAT (' ',/,'*** SDTRAN Error 3: Identical data points',/,
     +       '    NDP =',I5,',  IP1 =',I5,',  XD =',E11.3,',  YD =',
     +       E11.3)
 9030 FORMAT (' ',/,'*** SDTRAN Error 4: NDP outside its valid',
     +       ' range',/,'    NDP =',I5)
 9040 FORMAT (' ',/,'*** SDTRAN Error 5: ',
     +       'Invalid data structure (LIST,LPTR,LEND)',/)
      END


      SUBROUTINE SDTRCH(NDP,XD,YD, NT,IPT,NL,IPL,IERTM,IERTL,
     +                  LIST,LPTR,LEND,LTRI,NEAR,NEXT,DIST)
*
* Basic triangulation in the convex hull of a scattered data point
* set in a plane
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine triangulates the data area that is a convex hull
* of the scattered data points in the x-y plane.  It divides the
* data area into a number of triangles and determines line segments
* that form the border of the data area.
*
* This subroutine depends on the TRIPACK package of ACM Algorithm
* 751 by R. J. Renka.  It calls the TRMESH and TRLIST subroutines
* included in the package.  The TRMESH subroutine in turn calls
* either directly or indirectly 12 other subprograms included in
* the package.
*
* The input arguments are
*   NDP   = number of data points (must be greater than 3),
*   XD    = array of dimension NDP containing the x
*           coordinates of the data points,
*   YD    = array of dimension NDP containing the y
*           coordinates of the data points.
*
* The output arguments are
*   NT    = number of triangles (its maximum is 2*NDP-5),
*   IPT   = two-dimensional integer array of dimension
*           (3,NT), where the point numbers of the vertexes
*           of the ITth triangle are to be stored counter-
*           clockwise in the ITth column, where IT = 1, 2,
*           ..., NT,
*   NL    = number of border line segments (its maximum is
*           NDP),
*   IPL   = two-dimensional integer array of dimension
*           (2,NL), where the point numbers of the end
*           points of the (IL)th border line segment are to
*           be stored counterclockwise in the ILth column,
*           where IL = 1, 2, ..., NL, with the line segments
*           stored counterclockwise,
*   IERTM = error flag from the TRMESH subroutine,
*         =  0 for no errors
*         = -1 for NDP = 3 or less
*         = -2 for the first three collinear data points,
*         =  L for the Lth data point identical to some
*            Mth data point, M > L.
*   IERTL = error flag from the TRLIST subroutine,
*         = 0 for no errors
*         = 1 for invalid NCC, NDP, or NROW value.
*         = 2 for invalid data structure (LIST,LPTR,LEND).
*
* The other arguments are
*   LIST  = integer array of dimension 6*NDP USED internally
*           as a work area,
*   LPTR  = integer array of dimension 6*NDP USED internally
*           as a work area,
*   LEND  = integer array of dimension NDP USED internally as
*           a work area,
*   LTRI  = two-dimensional integer array of dimension 12*NDP
*           used internally as a work area.
*
* agebhard@uni-klu.ac.at: added from new TRIPACK:
*   NEAR, NEXT, DIST work arrays from TRMESH, size NDP
*
* Specification statements
*     .. Parameters ..
      INTEGER          NCC,NROW
      PARAMETER        (NCC=0,NROW=6)
*     ..
*     .. Scalar Arguments ..
      INTEGER          IERTL,IERTM,NDP,NL,NT,NEAR(NDP),NEXT(NDP)
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP),DIST(NDP)
      INTEGER          IPL(2,*),IPT(3,*),LEND(NDP),LIST(*),LPTR(*),
     +                 LTRI(NROW,*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,I1,I2,IL,IL1,IL2,IPL11,IPL21,J,LNEW
*     ..
*     .. Local Arrays ..
      INTEGER          LCC(1),LCT(1)
*     ..
*     .. External Subroutines ..
      EXTERNAL         TRLIST,TRMESH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
* Performs basic triangulation.
      CALL TRMESH(NDP,XD,YD, LIST,LPTR,LEND,LNEW,NEAR,NEXT,DIST,IERTM)
      IF (IERTM.NE.0) RETURN
      CALL TRLIST(NCC,LCC,NDP,LIST,LPTR,LEND,NROW, NT,LTRI,LCT,IERTL)
      IF (IERTL.NE.0) RETURN
* Extracts the triangle data from the LTRI array and set the IPT
* array.
      DO 20 J = 1,NT
          DO 10 I = 1,3
              IPT(I,J) = LTRI(I,J)
   10     CONTINUE
   20 CONTINUE
* Extracts the border-line-segment data from the LTRI array and
* set the IPL array.
      IL = 0
      DO 50 J = 1,NT
          DO 30 I = 1,3
              IF (LTRI(I+3,J).LE.0) GO TO 40
   30     CONTINUE
          GO TO 50
   40     IL = IL + 1
          I1 = MOD(I,3) + 1
          I2 = MOD(I+1,3) + 1
          IPL(1,IL) = LTRI(I1,J)
          IPL(2,IL) = LTRI(I2,J)
   50 CONTINUE
      NL = IL
* Sorts the IPL array.
      DO 80 IL1 = 1,NL - 1
          DO 60 IL2 = IL1 + 1,NL
              IF (IPL(1,IL2).EQ.IPL(2,IL1)) GO TO 70
   60     CONTINUE
   70     IPL11 = IPL(1,IL1+1)
          IPL21 = IPL(2,IL1+1)
          IPL(1,IL1+1) = IPL(1,IL2)
          IPL(2,IL1+1) = IPL(2,IL2)
          IPL(1,IL2) = IPL11
          IPL(2,IL2) = IPL21
   80 CONTINUE
      RETURN
      END


      SUBROUTINE SDTRTT(NDP,XD,YD, NT,IPT,NL,IPL, ITL)
*
* Removal of thin triangles along the border line of triangulation
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine removes thin triangles along the border line of
* triangulation.
*
* The input arguments are
*   NDP = number of data points (must be greater than 3),
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points.
*
* The input and output arguments are
*   NT  = number of triangles (its maximum is 2*NDP-5),
*   IPT = two-dimensional integer array of dimension
*         (3,NT), where the point numbers of the vertexes
*         of the ITth triangle are to be stored counter-
*         clockwise in the ITth column, where IT = 1, 2,
*         ..., NT,
*   NL  = number of border line segments (its maximum is
*         NDP),
*   IPL = two-dimensional integer array of dimension
*         (2,NL), where the point numbers of the end
*         points of the (IL)th border line segment are to
*         be stored counterclockwise in the ILth column,
*         where IL = 1, 2, ..., NL, with the line segments
*         stored counterclockwise.
*
* The other argument is
*   ITL = integer array of dimension NDP used internally as
*         a work area.
*
* The constants in the PARAMETER statement below are
*   HBRMN = minimum value of the height-to-bottom ratio of a
*           triangle along the border line of the data area,
*   NRRTT = number of repetitions in thin triangle removal.
* The constant values have been selected empirically.
*
* Specification statements
*     .. Parameters ..
      DOUBLE PRECISION             HBRMN
      INTEGER          NRRTT
      PARAMETER        (HBRMN=0.10,NRRTT=5)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),YD(NDP)
      INTEGER          IPL(2,*),IPT(3,*),ITL(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             HBR,U1,U2,U3,V1,V2,V3
      INTEGER          IL,IL0,IL00,IL1,ILP1,ILR1,IP1,IP2,IP3,IPL1,IPL2,
     +                 IREP,IT,IT0,ITP1,IV,IVP1,MODIF,NL0
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION             DSQF,VPDT
*     ..
* Statement Function definitions 
      DSQF(U1,V1,U2,V2) = (U2-U1)**2 + (V2-V1)**2
      VPDT(U1,V1,U2,V2,U3,V3) = (V3-V1)* (U2-U1) - (U3-U1)* (V2-V1)
*     ..
* Triangle numbers of triangles that share line segments with the
* border line.
      DO 20 IL = 1,NL
          IPL1 = IPL(1,IL)
          IPL2 = IPL(2,IL)
          DO 10 IT = 1,NT
              IF (IPL1.EQ.IPT(1,IT) .OR. IPL1.EQ.IPT(2,IT) .OR.
     +            IPL1.EQ.IPT(3,IT)) THEN
                  IF (IPL2.EQ.IPT(1,IT) .OR. IPL2.EQ.IPT(2,IT) .OR.
     +                IPL2.EQ.IPT(3,IT)) THEN
                      ITL(IL) = IT
                      GO TO 20
                  END IF
              END IF
   10     CONTINUE
   20 CONTINUE
* Removes thin triangles that share line segments with the border
* line.
      DO 130 IREP = 1,NRRTT
          MODIF = 0
          NL0 = NL
          IL = 0
          DO 120 IL0 = 1,NL0
              IL = IL + 1
              IP1 = IPL(1,IL)
              IP2 = IPL(2,IL)
              IT = ITL(IL)
* Calculates the height-to-bottom ratio of the triangle.
              IF (IPT(1,IT).NE.IP1 .AND. IPT(1,IT).NE.IP2) THEN
                  IP3 = IPT(1,IT)
              ELSE IF (IPT(2,IT).NE.IP1 .AND. IPT(2,IT).NE.IP2) THEN
                  IP3 = IPT(2,IT)
              ELSE
                  IP3 = IPT(3,IT)
              END IF
              HBR = VPDT(XD(IP1),YD(IP1),XD(IP2),YD(IP2),XD(IP3),
     +              YD(IP3))/DSQF(XD(IP1),YD(IP1),XD(IP2),YD(IP2))
              IF (HBR.LT.HBRMN) THEN
                  MODIF = 1
* Removes this triangle when applicable.
                  ITP1 = IT + 1
                  DO 30 IT0 = ITP1,NT
                      IPT(1,IT0-1) = IPT(1,IT0)
                      IPT(2,IT0-1) = IPT(2,IT0)
                      IPT(3,IT0-1) = IPT(3,IT0)
   30             CONTINUE
                  NT = NT - 1
                  DO 40 IL00 = 1,NL
                      IF (ITL(IL00).GT.IT) ITL(IL00) = ITL(IL00) - 1
   40             CONTINUE
* Replaces the border line segment with two new line segments.
                  IF (IL.LT.NL) THEN
                      ILP1 = IL + 1
                      DO 50 ILR1 = ILP1,NL
                          IL1 = NL + ILP1 - ILR1
                          IPL(1,IL1+1) = IPL(1,IL1)
                          IPL(2,IL1+1) = IPL(2,IL1)
                          ITL(IL1+1) = ITL(IL1)
   50                 CONTINUE
                  END IF
* - Adds the first new line segment.
                  IPL(1,IL) = IP1
                  IPL(2,IL) = IP3
                  DO 70 IT0 = 1,NT
                      DO 60 IV = 1,3
                          IF (IPT(IV,IT0).EQ.IP1 .OR.
     +                        IPT(IV,IT0).EQ.IP3) THEN
                              IVP1 = MOD(IV,3) + 1
                              IF (IPT(IVP1,IT0).EQ.IP1 .OR.
     +                            IPT(IVP1,IT0).EQ.IP3) GO TO 80
                          END IF
   60                 CONTINUE
   70             CONTINUE
   80             ITL(IL) = IT0
* - Adds the second new line segment.
                  IL = IL + 1
                  IPL(1,IL) = IP3
                  IPL(2,IL) = IP2
                  DO 100 IT0 = 1,NT
                      DO 90 IV = 1,3
                          IF (IPT(IV,IT0).EQ.IP3 .OR.
     +                        IPT(IV,IT0).EQ.IP2) THEN
                              IVP1 = MOD(IV,3) + 1
                              IF (IPT(IVP1,IT0).EQ.IP3 .OR.
     +                            IPT(IVP1,IT0).EQ.IP2) GO TO 110
                          END IF
   90                 CONTINUE
  100             CONTINUE
  110             ITL(IL) = IT0
                  NL = NL + 1
              END IF
  120     CONTINUE
          IF (MODIF.EQ.0) RETURN
  130 CONTINUE
      RETURN
      END


      SUBROUTINE SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP)
*
* Partial derivatives for bivariate interpolation and surface
* fitting for scattered data
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine estimates partial derivatives of the first and
* second orders at the data points for bivariate interpolation
* and surface fitting for scattered data.  In most cases, this
* subroutine has the accuracy of a cubic (third-degree)
* polynomial.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z values
*          at the data points.
*
* The output argument is
*   PDD  = two-dimensional array of dimension 5*NDP, where
*          the estimated zx, zy, zxx, zxy, and zyy values
*          at the IDPth data point are to be stored in the
*          IDPth row, where IDP = 1, 2, ..., NDP.
*
* The other arguments are
*   CF3  = two-dimensional array of dimension 9*NDP used
*          internally as a work area,
*   CFL1 = two-dimensional array of dimension 2*NDP used
*          internally as a work area,
*   DSQ  = array of dimension NDP used internally as a work
*          area,
*   IDSQ = integer array of dimension NDP used internally
*          as a work area,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          used internally as a work area,
*   NCP  = integer array of dimension NDP used internally
*          as a work area.
*
* The constant in the first PARAMETER statement below is
*   NPEMX = maximum number of primary estimates.
* The constant value has been selected empirically.
*
* The constants in the second PARAMETER statement below are
*   NPEAMN = minimum number of primary estimates,
*   NPEAMX = maximum number of primary estimates when
*            additional primary estimates are added.
* The constant values have been selected empirically.
*
* This subroutine calls the SDCLDP, SDCF3P, and SDLS1P
* subroutines.
*
*
* Specification statements
*     .. Parameters ..
      INTEGER          NPEMX
      PARAMETER        (NPEMX=25)
      INTEGER          NPEAMN,NPEAMX
      PARAMETER        (NPEAMN=3,NPEAMX=6)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CF3(9,NDP),CFL1(2,NDP),DSQ(NDP),
     +                 PDD(5,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IDSQ(NDP),IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A01,A02,A03,A10,A11,A12,A20,A21,A30,
     +                 ALPWT,ANPE,ANPEM1,SMWTF,SMWTI,WTF,WTI,X,Y,ZX,ZY
      INTEGER          IDP1,IDP2,IDPI,IDPPE1,IMN,IPE,IPE1,J,J1,J2,JJ,
     +                 JMN,K,NCP2,NCP2P1,NPE
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             AMPDPE(5),PDDIF(5),PDDII(5),
     +                 PDPE(5,NPEMX),PWT(NPEMX),RVWT(NPEMX),SSPDPE(5)
      INTEGER          IDPPE(NPEMX),IPCPE(10,NPEMX)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDCF3P,SDCLDP,SDLS1P
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        EXP,DBLE
*     ..
* Calculation
* Selects, at each of the data points, nine data points closest
* to the data point in question.
      CALL SDCLDP(NDP,XD,YD, IPC, DSQ,IDSQ)
* Fits, at each of the data points, a cubic (third-degree)
* polynomial to z values at the 10 data points that consist of
* the data point in question and 9 data points closest to it.
      CALL SDCF3P(NDP,XD,YD,ZD,IPC, CF3,NCP)
* Performs, at each of the data points, the least-squares fit of
* a plane to z values at the 10 data points.
      CALL SDLS1P(NDP,XD,YD,ZD,IPC,NCP, CFL1)
* Outermost DO-loop with respect to the data point
      DO 310 IDP1 = 1,NDP
* Selects data point sets for sets of primary estimates of partial
* derivatives.
* - Selects a candidate.
          NPE = 0
          DO 80 IDP2 = 1,NDP
              NCP2 = NCP(IDP2)
              NCP2P1 = NCP2 + 1
              IF (IDP2.EQ.IDP1) GO TO 20
              DO 10 J = 1,NCP2
                  IF (IPC(J,IDP2).EQ.IDP1) GO TO 20
   10         CONTINUE
              GO TO 80
   20         IPCPE(1,NPE+1) = IDP2
              DO 30 J = 1,NCP2
                  IPCPE(J+1,NPE+1) = IPC(J,IDP2)
   30         CONTINUE
              DO 50 J1 = 1,NCP2
                  JMN = J1
                  IMN = IPCPE(JMN,NPE+1)
                  DO 40 J2 = J1,NCP2P1
                      IF (IPCPE(J2,NPE+1).LT.IMN) THEN
                          JMN = J2
                          IMN = IPCPE(JMN,NPE+1)
                      END IF
   40             CONTINUE
                  IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
                  IPCPE(J1,NPE+1) = IMN
   50         CONTINUE
* - Checks whether or not the candidate has already been included.
              IF (NPE.GT.0) THEN
                  DO 70 IPE1 = 1,NPE
                      IDPPE1 = IDPPE(IPE1)
                      IF (NCP2.NE.NCP(IDPPE1)) GO TO 70
                      DO 60 J = 1,NCP2P1
                          IF (IPCPE(J,NPE+1).NE.
     +                        IPCPE(J,IPE1)) GO TO 70
   60                 CONTINUE
                      GO TO 80
   70             CONTINUE
              END IF
              NPE = NPE + 1
              IDPPE(NPE) = IDP2
              IF (NPE.GE.NPEMX) GO TO 90
   80     CONTINUE
   90     CONTINUE
* Adds additional closest data points when necessary.
          IF (NPE.LT.NPEAMN) THEN
              DO 150 JJ = 1,9
                  IDP2 = IPC(JJ,IDP1)
                  NCP2 = NCP(IDP2)
                  NCP2P1 = NCP2 + 1
                  IPCPE(1,NPE+1) = IDP2
                  DO 100 J = 1,NCP2
                      IPCPE(J+1,NPE+1) = IPC(J,IDP2)
  100             CONTINUE
                  DO 120 J1 = 1,NCP2
                      JMN = J1
                      IMN = IPCPE(JMN,NPE+1)
                      DO 110 J2 = J1,NCP2P1
                          IF (IPCPE(J2,NPE+1).LT.IMN) THEN
                              JMN = J2
                              IMN = IPCPE(JMN,NPE+1)
                          END IF
  110                 CONTINUE
                      IPCPE(JMN,NPE+1) = IPCPE(J1,NPE+1)
                      IPCPE(J1,NPE+1) = IMN
  120             CONTINUE
                  IF (NPE.GT.0) THEN
                      DO 140 IPE1 = 1,NPE
                          IDPPE1 = IDPPE(IPE1)
                          IF (NCP2.NE.NCP(IDPPE1)) GO TO 140
                          DO 130 J = 1,NCP2P1
                              IF (IPCPE(J,NPE+1).NE.
     +                            IPCPE(J,IPE1)) GO TO 140
  130                     CONTINUE
                          GO TO 150
  140                 CONTINUE
                  END IF
                  NPE = NPE + 1
                  IDPPE(NPE) = IDP2
                  IF (NPE.GE.NPEAMX) GO TO 160
  150         CONTINUE
          END IF
  160     CONTINUE
* Calculates the primary estimates of partial derivatives.
          X = XD(IDP1)
          Y = YD(IDP1)
          DO 170 IPE = 1,NPE
              IDPI = IDPPE(IPE)
              A10 = CF3(1,IDPI)
              A20 = CF3(2,IDPI)
              A30 = CF3(3,IDPI)
              A01 = CF3(4,IDPI)
              A11 = CF3(5,IDPI)
              A21 = CF3(6,IDPI)
              A02 = CF3(7,IDPI)
              A12 = CF3(8,IDPI)
              A03 = CF3(9,IDPI)
              PDPE(1,IPE) = A10 + X* (2.0*A20+X*3.0*A30) +
     +                      Y* (A11+2.0*A21*X+A12*Y)
              PDPE(2,IPE) = A01 + Y* (2.0*A02+Y*3.0*A03) +
     +                      X* (A11+2.0*A12*Y+A21*X)
              PDPE(3,IPE) = 2.0*A20 + 6.0*A30*X + 2.0*A21*Y
              PDPE(4,IPE) = A11 + 2.0*A21*X + 2.0*A12*Y
              PDPE(5,IPE) = 2.0*A02 + 6.0*A03*Y + 2.0*A12*X
  170     CONTINUE
          IF (NPE.EQ.1) GO TO 290
* Weighted values of partial derivatives (through the statement
* labeled 280 + 1)
* Calculates the probability weight.
          ANPE = DBLE(NPE)
          ANPEM1 = DBLE(NPE-1)
          DO 190 K = 1,5
              AMPDPE(K) = 0.0
              SSPDPE(K) = 0.0
              DO 180 IPE = 1,NPE
                  AMPDPE(K) = AMPDPE(K) + PDPE(K,IPE)
                  SSPDPE(K) = SSPDPE(K) + PDPE(K,IPE)**2
  180         CONTINUE
              AMPDPE(K) = AMPDPE(K)/ANPE
              SSPDPE(K) = (SSPDPE(K)-ANPE*AMPDPE(K)**2)/ANPEM1
  190     CONTINUE
          DO 210 IPE = 1,NPE
              ALPWT = 0.0
              DO 200 K = 1,5
                  IF (SSPDPE(K).NE.0.0) ALPWT = ALPWT +
     +                ((PDPE(K,IPE)-AMPDPE(K))**2)/SSPDPE(K)
  200         CONTINUE
              PWT(IPE) = EXP(-ALPWT/2.0)
  210     CONTINUE
* Calculates the reciprocal of the volatility weight.
          DO 220 IPE = 1,NPE
              IDPI = IDPPE(IPE)
              ZX = CFL1(1,IDPI)
              ZY = CFL1(2,IDPI)
              RVWT(IPE) = ((PDPE(1,IPE)-ZX)**2+ (PDPE(2,IPE)-ZY)**2)*
     +                    (PDPE(3,IPE)**2+2.0*PDPE(4,IPE)**2+
     +                    PDPE(5,IPE)**2)
*             ZXX=0.0
*             ZXY=0.0
*             ZYY=0.0
*             RVWT(IPE)=((PDPE(1,IPE)-ZX)**2+(PDPE(2,IPE)-ZY)**2)
*    +                 *((PDPE(3,IPE)-ZXX)**2+2.0*(PDPE(4,IPE)-ZXY)**2
*    +                  +(PDPE(5,IPE)-ZYY)**2)
  220     CONTINUE
* Calculates the weighted values of partial derivatives.
          DO 230 K = 1,5
              PDDIF(K) = 0.0
              PDDII(K) = 0.0
  230     CONTINUE
          SMWTF = 0.0
          SMWTI = 0.0
          DO 260 IPE = 1,NPE
              IF (RVWT(IPE).GT.0.0) THEN
                  WTF = PWT(IPE)/RVWT(IPE)
                  DO 240 K = 1,5
                      PDDIF(K) = PDDIF(K) + PDPE(K,IPE)*WTF
  240             CONTINUE
                  SMWTF = SMWTF + WTF
              ELSE
                  WTI = PWT(IPE)
                  DO 250 K = 1,5
                      PDDII(K) = PDDII(K) + PDPE(K,IPE)*WTI
  250             CONTINUE
                  SMWTI = SMWTI + WTI
              END IF
  260     CONTINUE
          IF (SMWTI.LE.0.0) THEN
              DO 270 K = 1,5
                  PDD(K,IDP1) = PDDIF(K)/SMWTF
  270         CONTINUE
          ELSE
              DO 280 K = 1,5
                  PDD(K,IDP1) = PDDII(K)/SMWTI
  280         CONTINUE
          END IF
          GO TO 310
* Only one qualified point set
  290     DO 300 K = 1,5
              PDD(K,IDP1) = PDPE(K,1)
  300     CONTINUE
  310 CONTINUE
      RETURN
      END


      SUBROUTINE SDCLDP(NDP,XD,YD, IPC, DSQ,IDSQ)
*
* Closest data points
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine selects, at each of the data points, nine data
* points closest to it.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points.
*
* The output argument is
*   IPC  = two-dimensional integer array of dimension 9*NDP,
*          where the point numbers of nine data points closest
*          to the IDPth data point, in an ascending order of
*          the distance from the IDPth point, are to be
*          stored in the IDPth column, where IDP = 1, 2,
*          ..., NDP.
*
* The other arguments are
*   DSQ  = array of dimension NDP used as a work area,
*   IDSQ = integer array of dimension NDP used as a work
*          area.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             DSQ(NDP),XD(NDP),YD(NDP)
      INTEGER          IDSQ(NDP),IPC(9,NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             DSQMN
      INTEGER          IDP,IDSQMN,JDP,JDPMN,JDSQMN,JIPC,JIPCMX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     ..
* DO-loop with respect to the data point number
      DO 50 IDP = 1,NDP
* Calculates the distance squared for all data points from the
* IDPth data point and stores the data point number and the
* calculated results in the IDSQ and DSQ arrays, respectively.
          DO 10 JDP = 1,NDP
              IDSQ(JDP) = JDP
              DSQ(JDP) = (XD(JDP)-XD(IDP))**2 + (YD(JDP)-YD(IDP))**2
   10     CONTINUE
* Sorts the IDSQ and DSQ arrays in such a way that the IDPth
* point is in the first element in each array.
          IDSQ(IDP) = 1
          DSQ(IDP) = DSQ(1)
          IDSQ(1) = IDP
          DSQ(1) = 0.0
* Selects nine data points closest to the IDPth data point and
* stores the data point numbers in the IPC array.
          JIPCMX = MIN(NDP-1,10)
          DO 30 JIPC = 2,JIPCMX
              JDSQMN = JIPC
              DSQMN = DSQ(JIPC)
              JDPMN = JIPC + 1
              DO 20 JDP = JDPMN,NDP
                  IF (DSQ(JDP).LT.DSQMN) THEN
                      JDSQMN = JDP
                      DSQMN = DSQ(JDP)
                  END IF
   20         CONTINUE
              IDSQMN = IDSQ(JDSQMN)
              IDSQ(JDSQMN) = IDSQ(JIPC)
              DSQ(JDSQMN) = DSQ(JIPC)
              IDSQ(JIPC) = IDSQMN
   30     CONTINUE
          DO 40 JIPC = 1,9
              IPC(JIPC,IDP) = IDSQ(JIPC+1)
   40     CONTINUE
   50 CONTINUE
      RETURN
      END


      SUBROUTINE SDCF3P(NDP,XD,YD,ZD,IPC, CF,NCP)
*
* Coefficients of the third-degree polynomial for z(x,y)
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine calculates, for each data point, coefficients
* of the third-degree polynomial for z(x,y) fitted to the set of
* 10 data points consisting of the data point in question and
* nine data points closest to it.  When the condition number of
* the matrix associated with the 10 data point set is too large,
* this subroutine calculates coefficients of the second-degree
* polynomial fitted to the set of six data points consisting of
* the data point in question and five data points closest to it.
* When the condition number of the matrix associated with the six
* data point set is too large, this subroutine calculates
* coefficients of the first-degree polynomial fitted to the set of
* three data points closest to the data point in question.  When
* the condition number of the matrix associated with the three data
* point set is too large, this subroutine calculates coefficients
* of the first-degree polynomial fitted to the set of two data
* points consisting of the data point in question and one data
* point closest to it, assuming that the plane represented by the
* polynomial is horizontal in the direction which is at right
* angles to the line connecting the two data points.
*
* The input arguments are
*   NDP = number of data points,
*   XD  = array of dimension NDP containing the x
*         coordinates of the data points,
*   YD  = array of dimension NDP containing the y
*         coordinates of the data points,
*   ZD  = array of dimension NDP containing the z values
*         at the data points,
*   IPC = two-dimensional integer array of dimension
*         9*NDP containing the point numbers of 9 data
*         points closest to the IDPth data point in the
*         IDPth column, where IDP = 1, 2, ..., NDP.
*
* The output arguments are
*   CF  = two-dimensional array of dimension 9*NDP,
*         where the coefficients of the polynomial
*         (a10, a20, a30, a01, a11, a21, a02, a12, a03)
*         calculated at the IDPth data point are to be
*         stored in the IDPth column, where IDP = 1, 2,
*         ..., NDP,
*   NCP = integer array of dimension NDP, where the numbers
*         of the closest points used are to be stored.
*
* The constant in the first PARAMETER statement below is
*   CNRMX = maximum value of the ratio of the condition
*           number of the matrix associated with the point
*           set to the number of points.
* The constant value has been selected empirically.
*
* The N1, N2, and N3 constants in the second PARAMETER statement
* are the numbers of the data points used to determine the first-,
* second-, and third-degree polynomials, respectively.
*
* This subroutine calls the SDLEQN subroutine.
*
*
* Specification statements
*     .. Parameters ..
      DOUBLE PRECISION             CNRMX
      PARAMETER        (CNRMX=1.5E+04)
      INTEGER          N1,N2,N3
      PARAMETER        (N1=3,N2=6,N3=10)
*     ..
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CF(9,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             CN,DET,X,X1,X2,Y,Y1,Y2,Z1,Z2
      INTEGER          I,IDP,IDPI,J
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             AA1(N1,N1),AA2(N2,N2),AA3(N3,N3),
     +                 B(N3),CFI(N3),EE(N3,N3),ZZ(N3,N3)
      INTEGER          K(N3)
*     ..
*     .. External Subroutines ..
      EXTERNAL         SDLEQN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     ..
* Main DO-loop with respect to the data point
      DO 60 IDP = 1,NDP
          DO 10 J = 1,9
              CF(J,IDP) = 0.0
   10     CONTINUE
* Calculates the coefficients of the set of linear equations
* with the 10-point data point set.
          DO 20 I = 1,N3
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              AA3(I,1) = 1.0
              AA3(I,2) = X
              AA3(I,3) = X*X
              AA3(I,4) = X*X*X
              AA3(I,5) = Y
              AA3(I,6) = X*Y
              AA3(I,7) = X*X*Y
              AA3(I,8) = Y*Y
              AA3(I,9) = X*Y*Y
              AA3(I,10) = Y*Y*Y
              B(I) = ZD(IDPI)
   20     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N3,AA3,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* third-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N3)) THEN
                  DO 30 J = 2,N3
                      CF(J-1,IDP) = CFI(J)
   30             CONTINUE
                  NCP(IDP) = N3 - 1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 6-point data point set.
          DO 40 I = 1,N2
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              AA2(I,1) = 1.0
              AA2(I,2) = X
              AA2(I,3) = X*X
              AA2(I,4) = Y
              AA2(I,5) = X*Y
              AA2(I,6) = Y*Y
              B(I) = ZD(IDPI)
   40     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N2,AA2,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* second-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N2)) THEN
                  CF(1,IDP) = CFI(2)
                  CF(2,IDP) = CFI(3)
                  CF(4,IDP) = CFI(4)
                  CF(5,IDP) = CFI(5)
                  CF(7,IDP) = CFI(6)
                  NCP(IDP) = N2 - 1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 3-point data point set.
          DO 50 I = 1,N1
              IDPI = IPC(I,IDP)
              X = XD(IDPI)
              Y = YD(IDPI)
              AA1(I,1) = 1.0
              AA1(I,2) = X
              AA1(I,3) = Y
              B(I) = ZD(IDPI)
   50     CONTINUE
* Solves the set of linear equations.
          CALL SDLEQN(N1,AA1,B, CFI,DET,CN, K,EE,ZZ)
* Stores the calculated results as the coefficients of the
* first-degree polynomial when applicable.
          IF (DET.NE.0.0) THEN
              IF (CN.LE.CNRMX*DBLE(N1)) THEN
                  CF(1,IDP) = CFI(2)
                  CF(4,IDP) = CFI(3)
                  NCP(IDP) = N1
                  GO TO 60
              END IF
          END IF
* Calculates the coefficients of the set of linear equations
* with the 2-point data point set when applicable.
          IDPI = IDP
          X1 = XD(IDPI)
          Y1 = YD(IDPI)
          Z1 = ZD(IDPI)
          IDPI = IPC(1,IDP)
          X2 = XD(IDPI)
          Y2 = YD(IDPI)
          Z2 = ZD(IDPI)
          CF(1,IDP) = (X2-X1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          CF(4,IDP) = (Y2-Y1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          NCP(IDP) = 1
   60 CONTINUE
      RETURN
      END


      SUBROUTINE SDLEQN(N,AA,B, X,DET,CN, K,EE,ZZ)
*
* Solution of a set of linear equations
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine solves a set of linear equations.
*
* The input arguments are
*   N   = number of linear equations,
*   AA  = two-dimensional array of dimension N*N
*         containing the coefficients of the equations,
*   B   = array of dimension N containing the constant
*         values in the right-hand side of the equations.
*
* The output arguments are
*   X   = array of dimension N, where the solution is
*         to be stored,
*   DET = determinant of the AA array,
*   CN  = condition number of the AA matrix.
*
* The other arguments are
*   K   = integer array of dimension N used internally
*         as the work area,
*   EE  = two-dimensional array of dimension N*N used
*         internally as the work area,
*   ZZ  = two-dimensional array of dimension N*N used
*         internally as the work area.
*
*
* Specification statements
*     .. Scalar Arguments ..
      DOUBLE PRECISION             CN,DET
      INTEGER          N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             AA(N,N),B(N),EE(N,N),X(N),ZZ(N,N)
      INTEGER          K(N)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             AAIIJ,AAIJIJ,AAIJMX,AAMX,SA,SZ
      INTEGER          I,IJ,IJP1,IJR,J,JJ,JMX,KJMX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        ABS,SQRT
*     ..
* Calculation
* Initial setting
      DO 10 J = 1,N
          K(J) = J
   10 CONTINUE
      DO 30 I = 1,N
          DO 20 J = 1,N
              EE(I,J) = 0.0
   20     CONTINUE
          EE(I,I) = 1.0
   30 CONTINUE
* Calculation of inverse matrix of AA
      DO 110 IJ = 1,N
* Finds out the element having the maximum absolute value in the
* IJ th row.
          AAMX = ABS(AA(IJ,IJ))
          JMX = IJ
          DO 40 J = IJ,N
              IF (ABS(AA(IJ,J)).GT.AAMX) THEN
                  AAMX = ABS(AA(IJ,J))
                  JMX = J
              END IF
   40     CONTINUE
* Switches two columns in such a way that the element with the
* maximum value is on the diagonal.
          DO 50 I = 1,N
              AAIJMX = AA(I,IJ)
              AA(I,IJ) = AA(I,JMX)
              AA(I,JMX) = AAIJMX
   50     CONTINUE
          KJMX = K(IJ)
          K(IJ) = K(JMX)
          K(JMX) = KJMX
* Makes the diagonal element to be unity.
          AAIJIJ = AA(IJ,IJ)
          IF (AAIJIJ.EQ.0.0) GO TO 210
          DO 60 J = IJ,N
              AA(IJ,J) = AA(IJ,J)/AAIJIJ
   60     CONTINUE
          DO 70 JJ = 1,N
              EE(IJ,JJ) = EE(IJ,JJ)/AAIJIJ
   70     CONTINUE
* Eliminates the lower left elements.
          IF (IJ.LT.N) THEN
              IJP1 = IJ + 1
              DO 100 I = IJP1,N
                  AAIIJ = AA(I,IJ)
                  DO 80 J = IJP1,N
                      AA(I,J) = AA(I,J) - AA(IJ,J)*AAIIJ
   80             CONTINUE
                  DO 90 JJ = 1,N
                      EE(I,JJ) = EE(I,JJ) - EE(IJ,JJ)*AAIIJ
   90             CONTINUE
  100         CONTINUE
          END IF
* Calculates the determinant.
          IF (IJ.EQ.1) THEN
              DET = 1.0
          END IF
          DET = DET*AAIJIJ* ((-1)** (IJ+JMX))
  110 CONTINUE
* Calculates the elements of the inverse matrix.
      DO 140 IJR = 1,N
          IJ = N + 1 - IJR
          IF (IJ.LT.N) THEN
              IJP1 = IJ + 1
              DO 130 J = IJP1,N
                  DO 120 JJ = 1,N
                      EE(IJ,JJ) = EE(IJ,JJ) - AA(IJ,J)*EE(J,JJ)
  120             CONTINUE
  130         CONTINUE
          END IF
  140 CONTINUE
      DO 160 J = 1,N
          I = K(J)
          DO 150 JJ = 1,N
              ZZ(I,JJ) = EE(J,JJ)
  150     CONTINUE
  160 CONTINUE
* Calculation of the condition number of AA
      SA = 0.0
      SZ = 0.0
      DO 180 I = 1,N
          DO 170 J = 1,N
              SA = SA + AA(I,J)*AA(J,I)
              SZ = SZ + ZZ(I,J)*ZZ(J,I)
  170     CONTINUE
  180 CONTINUE
      CN = SQRT(ABS(SA*SZ))
* Calculation of X vector
      DO 200 I = 1,N
          X(I) = 0.0
          DO 190 J = 1,N
              X(I) = X(I) + ZZ(I,J)*B(J)
  190     CONTINUE
  200 CONTINUE
      RETURN
* Special case where the determinant is zero
  210 DO 220 I = 1,N
          X(I) = 0.0
  220 CONTINUE
      DET = 0.0
      RETURN
      END


      SUBROUTINE SDLS1P(NDP,XD,YD,ZD,IPC,NCP, CFL1)
*
* Least squares fit of a linear surface (plane) to z(x,y) values
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine performs the least squares fit of a linear
* surface (plane) to a data point set consisting of the data
* point in question and several data points closest to it used
* in the SDCF3P subroutine.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x coordinates
*          of the data points,
*   YD   = array of dimension NDP containing the y coordinates
*          of the data points,
*   ZD   = array of dimension NDP containing the z values at
*          the data points,
*   IPC  = two-dimensional integer array of dimension 9*NDP
*          containing, in the IDPth column, point numbers of
*          nine data points closest to the IDPth data point,
*          where IDP = 1, 2, ..., NDP,
*   NCP  = integer array of dimension NDP containing the
*          numbers of the closest points used in the SDCF3P
*          subroutine.
*
* The output argument is
*   CFL1 = two-dimensional array of dimension 2*NDP, where
*          the coefficients (a10, a01) of the least squares
*          fit, first-degree polynomial calculated at the
*          IDPth data point are to be stored in the IDPth
*          column, where IDP = 1, 2, ..., NDP.
*
* Before this subroutine is called, the SDCF3P subroutine must
* have been called.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             CFL1(2,NDP),XD(NDP),YD(NDP),ZD(NDP)
      INTEGER          IPC(9,NDP),NCP(NDP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A11,A12,A22,AN,B1,B2,DLT,SX,SXX,SXY,
     +                 SXZ,SY,SYY,SYZ,SZ,X,X1,X2,Y,Y1,Y2,Z,Z1,Z2
      INTEGER          I,IDP,IDPI,NPLS
*     ..
* DO-loop with respect to the data point
      DO 30 IDP = 1,NDP
          NPLS = NCP(IDP) + 1
          IF (NPLS.EQ.2) GO TO 20
* Performs the least squares fit of a plane.
          SX = 0.0
          SY = 0.0
          SXX = 0.0
          SXY = 0.0
          SYY = 0.0
          SZ = 0.0
          SXZ = 0.0
          SYZ = 0.0
          DO 10 I = 1,NPLS
              IF (I.EQ.1) THEN
                  IDPI = IDP
              ELSE
                  IDPI = IPC(I-1,IDP)
              END IF
              X = XD(IDPI)
              Y = YD(IDPI)
              Z = ZD(IDPI)
              SX = SX + X
              SY = SY + Y
              SXX = SXX + X*X
              SXY = SXY + X*Y
              SYY = SYY + Y*Y
              SZ = SZ + Z
              SXZ = SXZ + X*Z
              SYZ = SYZ + Y*Z
   10     CONTINUE
          AN = NPLS
          A11 = AN*SXX - SX*SX
          A12 = AN*SXY - SX*SY
          A22 = AN*SYY - SY*SY
          B1 = AN*SXZ - SX*SZ
          B2 = AN*SYZ - SY*SZ
          DLT = A11*A22 - A12*A12
          CFL1(1,IDP) = (B1*A22-B2*A12)/DLT
          CFL1(2,IDP) = (B2*A11-B1*A12)/DLT
          GO TO 30
   20     IDPI = IDP
          X1 = XD(IDPI)
          Y1 = YD(IDPI)
          Z1 = ZD(IDPI)
          IDPI = IPC(1,IDP)
          X2 = XD(IDPI)
          Y2 = YD(IDPI)
          Z2 = ZD(IDPI)
          CFL1(1,IDP) = (X2-X1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
          CFL1(2,IDP) = (Y2-Y1)* (Z2-Z1)/ ((X2-X1)**2+ (Y2-Y1)**2)
   30 CONTINUE
      RETURN
      END


      SUBROUTINE SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI)
*
* Locating points in a scattered data point set
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine locates points in a scattered data point set in
* the x-y plane, i.e., determines to which triangle each of the
* points to be located belongs.  When a point to be located does
* not lie inside the data area, this subroutine determines the
* border line segment when the point lies in an outside rectangle,
* in an outside triangle, or in the overlap of two outside
* rectangles.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   NIP  = number of points to be located,
*   XI   = array of dimension NIP containing the x
*          coordinates of the points to be located,
*   YI   = array of dimension NIP containing the y
*          coordinates of the points to be located.
*
* The output arguments are
*   KTLI = integer array of dimension NIP, where the code
*          for the type of the piece of plane in which each
*          interpolated point lies is to be stored
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border line
*            segments
*        = 4 for a triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP, where the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the points to
*          be located are to be stored.
*
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP,NIP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             XD(NDP),XI(NIP),YD(NDP),YI(NIP)
      INTEGER          IPL(2,NL),IPT(3,NT),ITLI(NIP),KTLI(NIP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             U1,U2,U3,V1,V2,V3,X0,X1,X2,X3,Y0,Y1,
     +                             Y2,Y3
      INTEGER          IIP,IL1,IL2,ILII,IP1,IP2,IP3,ITII,ITLIPV,KTLIPV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION             SPDT,VPDT
*     ..
* Statement Function definitions 
      SPDT(U1,V1,U2,V2,U3,V3) = (U1-U3)* (U2-U3) + (V1-V3)* (V2-V3)
      VPDT(U1,V1,U2,V2,U3,V3) = (U1-U3)* (V2-V3) - (V1-V3)* (U2-U3)
*     ..
* Outermost DO-loop with respect to the points to be located
      DO 40 IIP = 1,NIP
          X0 = XI(IIP)
          Y0 = YI(IIP)
          IF (IIP.EQ.1) THEN
              KTLIPV = 0
              ITLIPV = 0
          ELSE
              KTLIPV = KTLI(IIP-1)
              ITLIPV = ITLI(IIP-1)
          END IF
* Checks if in the same inside triangle as previous.
          IF (KTLIPV.EQ.1) THEN
              ITII = ITLIPV
              IP1 = IPT(1,ITII)
              IP2 = IPT(2,ITII)
              IP3 = IPT(3,ITII)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF ((VPDT(X1,Y1,X2,Y2,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X2,Y2,X3,Y3,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X3,Y3,X1,Y1,X0,Y0).GE.0.0)) THEN
                  KTLI(IIP) = 1
                  ITLI(IIP) = ITII
                  GO TO 40
              END IF
          END IF
* Locates inside the data area.
          DO 10 ITII = 1,NT
              IP1 = IPT(1,ITII)
              IP2 = IPT(2,ITII)
              IP3 = IPT(3,ITII)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF ((VPDT(X1,Y1,X2,Y2,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X2,Y2,X3,Y3,X0,Y0).GE.0.0) .AND.
     +            (VPDT(X3,Y3,X1,Y1,X0,Y0).GE.0.0)) THEN
                  KTLI(IIP) = 1
                  ITLI(IIP) = ITII
                  GO TO 40
              END IF
   10     CONTINUE
* Locates outside the data area.
          DO 20 ILII = 1,NL
              IL1 = ILII
              IL2 = MOD(IL1,NL) + 1
              IP1 = IPL(1,IL1)
              IP2 = IPL(1,IL2)
              IP3 = IPL(2,IL2)
              X1 = XD(IP1)
              Y1 = YD(IP1)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF (VPDT(X1,Y1,X3,Y3,X0,Y0).LE.0.0) THEN
                  IF (VPDT(X1,Y1,X3,Y3,X2,Y2).LE.0.0) THEN
                      IF ((SPDT(X1,Y1,X0,Y0,X2,Y2).LE.0.0) .AND.
     +                    (SPDT(X3,Y3,X0,Y0,X2,Y2).LE.0.0)) THEN
                          KTLI(IIP) = 3
                          ITLI(IIP) = IL2
                          GO TO 40
                      END IF
                  END IF
                  IF (VPDT(X1,Y1,X3,Y3,X2,Y2).GE.0.0) THEN
                      IF ((SPDT(X1,Y1,X0,Y0,X2,Y2).GE.0.0) .AND.
     +                    (SPDT(X3,Y3,X0,Y0,X2,Y2).GE.0.0)) THEN
                          KTLI(IIP) = 4
                          ITLI(IIP) = IL2
                          GO TO 40
                      END IF
                  END IF
              END IF
   20     CONTINUE
          DO 30 ILII = 1,NL
              IL2 = ILII
              IP2 = IPL(1,IL2)
              IP3 = IPL(2,IL2)
              X2 = XD(IP2)
              Y2 = YD(IP2)
              X3 = XD(IP3)
              Y3 = YD(IP3)
              IF (VPDT(X2,Y2,X3,Y3,X0,Y0).LE.0.0) THEN
                  IF ((SPDT(X3,Y3,X0,Y0,X2,Y2).GE.0.0) .AND.
     +                (SPDT(X2,Y2,X0,Y0,X3,Y3).GE.0.0)) THEN
                      KTLI(IIP) = 2
                      ITLI(IIP) = IL2
                      GO TO 40
                  END IF
              END IF
   30     CONTINUE
   40 CONTINUE
      END


      SUBROUTINE SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD,NIP,XI,YI,KTLI,
     +                  ITLI, ZI, EXTRPI)
*
* Polynomials
* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package)
*
* Hiroshi Akima
* U.S. Department of Commerce, NTIA/ITS
* Version of 1995/05
*
* This subroutine determines a polynomial in x and y for each
* triangle or rectangle in the x-y plane and calculates the z
* value by evaluating the polynomial for the desired points,
* for bivariate interpolation and surface fitting for scattered
* data.
*
* The input arguments are
*   NDP  = number of data points,
*   XD   = array of dimension NDP containing the x
*          coordinates of the data points,
*   YD   = array of dimension NDP containing the y
*          coordinates of the data points,
*   ZD   = array of dimension NDP containing the z
*          values at the data points,
*   NT   = number of triangles,
*   IPT  = two-dimensional integer array of dimension 3*NT
*          containing the point numbers of the vertexes of
*          the triangles,
*   NL   = number of border line segments,
*   IPL  = two-dimensional integer array of dimension 2*NL
*          containing the point numbers of the end points of
*          the border line segments,
*   PDD  = two-dimensional array of dimension 5*NDP
*          containing the partial derivatives at the data
*          points,
*   NIP  = number of output points at which interpolation is
*          to be performed,
*   XI   = array of dimension NIP containing the x
*          coordinates of the output points,
*   YI   = array of dimension NIP containing the y
*          coordinates of the output points,
*   KTLI = integer array of dimension NIP, each element
*          containing the code for the type of the piece of
*          the plane in which each output point lies
*        = 1 for a triangle inside the data area
*        = 2 for a rectangle on the right-hand side of a
*            border line segment
*        = 3 for a triangle between two rectangles on the
*            right-hand side of two consecutive border
*            line segments
*        = 4 for the triangle which is an overlap of two
*            rectangles on the right-hand side of two
*            consecutive border line segments,
*   ITLI = integer array of dimension NIP containing the
*          triangle numbers or the (second) border line
*          segment numbers corresponding to the output
*          points.
*
* The output argument is
*   ZI   = array of dimension NIP, where the calculated z
*          values are to be stored.
*   EXTRPI = logical array of dimension NIP, indicating 
*            if a point resides outside the convex hull (and its Z value
*            has been extrapolated)
*
* Specification statements
*     .. Scalar Arguments ..
      INTEGER          NDP,NIP,NL,NT
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION             PDD(5,NDP),XD(NDP),XI(NIP),YD(NDP),
     +                 YI(NIP),ZD(NDP),ZI(NIP)
      INTEGER          IPL(2,NL),IPT(3,NT),ITLI(NIP),KTLI(NIP)
      LOGICAL          EXTRPI(NIP)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION             A,AA,AB,ACT2,AD,ADBC,AP,B,BB,BC,BDT2,
     +                 BP,C,CC,CD,
     +                 CP,D,DD,DLT,DP,DX,DY,E1,E2,G1,G2,H1,H2,H3,LUSQ,
     +                 LVSQ,P0,P00,P01,P02,P03,P04,P05,P1,P10,P11,P12,
     +                 P13,P14,P2,P20,P21,P22,P23,P3,P30,P31,P32,P4,P40,
     +                 P41,P5,P50,SPUV,U,V,WT1,WT2,X0,XII,Y0,YII,Z0,ZII,
     +                 ZII1,ZII2
      INTEGER          I,IDP,IIP,ILI,IR,ITLII,ITLIPV,K,KTLII,KTLIPV
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION             PD(5,3),X(3),Y(3),Z(3),ZU(3),ZUU(3),
     +                 ZUV(3),ZV(3),ZVV(3)
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
* Outermost DO-loop with respect to the output point
      DO 120 IIP = 1,NIP
          XII = XI(IIP)
          YII = YI(IIP)
          KTLII = KTLI(IIP)
          ITLII = ITLI(IIP)
          IF (IIP.EQ.1) THEN
              KTLIPV = 0
              ITLIPV = 0
          ELSE
              KTLIPV = KTLI(IIP-1)
              ITLIPV = ITLI(IIP-1)
          END IF
* Part 1.  Calculation of ZII by interpolation
          IF (KTLII.EQ.1) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the
* vertexes.
                  DO 20 I = 1,3
                      IDP = IPT(I,ITLII)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 10 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   10                 CONTINUE
   20             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = X(2) - X0
                  B = X(3) - X0
                  C = Y(2) - Y0
                  D = Y(3) - Y0
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -C/DLT
                  DP = A/DLT
* Converts the partial derivatives at the vertexes of the
* triangle for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 30 I = 1,3
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
   30             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P10 - P20
                  H2 = ZU(2) - P10 - ZUU(1)
                  H3 = ZUU(2) - ZUU(1)
                  P30 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P40 = -15.0*H1 + 7.0*H2 - H3
                  P50 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = Z(3) - P00 - P01 - P02
                  H2 = ZV(3) - P01 - ZVV(1)
                  H3 = ZVV(3) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  LUSQ = AA + CC
                  LVSQ = BB + DD
                  SPUV = AB + CD
                  P41 = 5.0*SPUV/LUSQ*P50
                  P14 = 5.0*SPUV/LVSQ*P05
                  H1 = ZV(2) - P01 - P11 - P41
                  H2 = ZUV(2) - P11 - 4.0*P41
                  P21 = 3.0*H1 - H2
                  P31 = -2.0*H1 + H2
                  H1 = ZU(3) - P10 - P11 - P14
                  H2 = ZUV(3) - P11 - 4.0*P14
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  E1 = (LVSQ-SPUV)/ ((LVSQ-SPUV)+ (LUSQ-SPUV))
                  E2 = 1.0 - E1
                  G1 = 5.0*E1 - 2.0
                  G2 = 1.0 - G1
                  H1 = 5.0* (E1* (P50-P41)+E2* (P05-P14)) + (P41+P14)
                  H2 = 0.5*ZVV(2) - P02 - P12
                  H3 = 0.5*ZUU(3) - P20 - P21
                  P22 = H1 + G1*H2 + G2*H3
                  P32 = H2 - P22
                  P23 = H3 - P22
              END IF
* Converts XII and YII to u-v system.
              DX = XII - X0
              DY = YII - Y0
              U = AP*DX + BP*DY
              V = CP*DX + DP*DY
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
              P1 = P10 + V* (P11+V* (P12+V* (P13+V*P14)))
              P2 = P20 + V* (P21+V* (P22+V*P23))
              P3 = P30 + V* (P31+V*P32)
              P4 = P40 + V*P41
              P5 = P50
              ZI(IIP) = P0 + U* (P1+U* (P2+U* (P3+U* (P4+U*P5))))
              EXTRPI(IIP) = .FALSE.
          END IF
* Part 2.  Calculation of ZII by extrapolation in the rectangle
          IF (KTLII.EQ.2) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the end
* points of the border line segment.
                  DO 50 I = 1,2
                      IDP = IPL(I,ITLII)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 40 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   40                 CONTINUE
   50             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = Y(2) - Y(1)
                  B = X(2) - X(1)
                  C = -B
                  D = A
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -BP
                  DP = AP
* Converts the partial derivatives at the end points of the
* border line segment for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 60 I = 1,2
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
   60             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P01 - P02
                  H2 = ZV(2) - P01 - ZVV(1)
                  H3 = ZVV(2) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = ZU(2) - P10 - P11
                  H2 = ZUV(2) - P11
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  P21 = 0.5* (ZUU(2)-ZUU(1))
              END IF
* Converts XII and YII to u-v system.
              DX = XII - X0
              DY = YII - Y0
              U = AP*DX + BP*DY
              V = CP*DX + DP*DY
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
              P1 = P10 + V* (P11+V* (P12+V*P13))
              P2 = P20 + V*P21
              ZI(IIP) = P0 + U* (P1+U*P2)
              EXTRPI(IIP) = .TRUE.
          END IF
* Part 3.  Calculation of ZII by extrapolation in the triangle
          IF (KTLII.EQ.3) THEN
* Calculates the coefficients when necessary.
              IF (KTLII.NE.KTLIPV .OR. ITLII.NE.ITLIPV) THEN
* Loads coordinate and partial derivative values at the vertex
* of the triangle.
                  IDP = IPL(1,ITLII)
                  X0 = XD(IDP)
                  Y0 = YD(IDP)
                  Z0 = ZD(IDP)
                  DO 70 K = 1,5
                      PD(K,1) = PDD(K,IDP)
   70             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z0
                  P10 = PD(1,1)
                  P01 = PD(2,1)
                  P20 = 0.5*PD(3,1)
                  P11 = PD(4,1)
                  P02 = 0.5*PD(5,1)
              END IF
* Converts XII and YII to U-V system.
              U = XII - X0
              V = YII - Y0
* Evaluates the polynomial.
              P0 = P00 + V* (P01+V*P02)
              P1 = P10 + V*P11
              ZI(IIP) = P0 + U* (P1+U*P20)
              EXTRPI(IIP) = .TRUE.
          END IF
* Part 4.  Calculation of ZII by extrapolation in the triangle
*          which is an overlap of two rectangles.
          IF (KTLII.EQ.4) THEN
* Calculates the coefficients.
              DO 110 IR = 1,2
                  IF (IR.EQ.1) THEN
                      ILI = MOD(ITLII+NL-2,NL) + 1
                  ELSE
                      ILI = ITLII
                  END IF
* Loads coordinate and partial derivative values at the end
* points of the border line segment.
                  DO 90 I = 1,2
                      IDP = IPL(I,ILI)
                      X(I) = XD(IDP)
                      Y(I) = YD(IDP)
                      Z(I) = ZD(IDP)
                      DO 80 K = 1,5
                          PD(K,I) = PDD(K,IDP)
   80                 CONTINUE
   90             CONTINUE
* Determines the coefficients for the coordinate system
* transformation from the x-y system to the u-v system
* and vice versa.
                  X0 = X(1)
                  Y0 = Y(1)
                  A = Y(2) - Y(1)
                  B = X(2) - X(1)
                  C = -B
                  D = A
                  AD = A*D
                  BC = B*C
                  DLT = AD - BC
                  AP = D/DLT
                  BP = -B/DLT
                  CP = -BP
                  DP = AP
* Converts the partial derivatives at the end points of the
* border line segment for the u-v coordinate system.
                  AA = A*A
                  ACT2 = 2.0*A*C
                  CC = C*C
                  AB = A*B
                  ADBC = AD + BC
                  CD = C*D
                  BB = B*B
                  BDT2 = 2.0*B*D
                  DD = D*D
                  DO 100 I = 1,2
                      ZU(I) = A*PD(1,I) + C*PD(2,I)
                      ZV(I) = B*PD(1,I) + D*PD(2,I)
                      ZUU(I) = AA*PD(3,I) + ACT2*PD(4,I) + CC*PD(5,I)
                      ZUV(I) = AB*PD(3,I) + ADBC*PD(4,I) + CD*PD(5,I)
                      ZVV(I) = BB*PD(3,I) + BDT2*PD(4,I) + DD*PD(5,I)
  100             CONTINUE
* Calculates the coefficients of the polynomial.
                  P00 = Z(1)
                  P10 = ZU(1)
                  P01 = ZV(1)
                  P20 = 0.5*ZUU(1)
                  P11 = ZUV(1)
                  P02 = 0.5*ZVV(1)
                  H1 = Z(2) - P00 - P01 - P02
                  H2 = ZV(2) - P01 - ZVV(1)
                  H3 = ZVV(2) - ZVV(1)
                  P03 = 10.0*H1 - 4.0*H2 + 0.5*H3
                  P04 = -15.0*H1 + 7.0*H2 - H3
                  P05 = 6.0*H1 - 3.0*H2 + 0.5*H3
                  H1 = ZU(2) - P10 - P11
                  H2 = ZUV(2) - P11
                  P12 = 3.0*H1 - H2
                  P13 = -2.0*H1 + H2
                  P21 = 0.5* (ZUU(2)-ZUU(1))
* Converts XII and YII to u-v system.
                  DX = XII - X0
                  DY = YII - Y0
                  U = AP*DX + BP*DY
                  V = CP*DX + DP*DY
* Evaluates the polynomial.
                  P0 = P00 + V* (P01+V* (P02+V* (P03+V* (P04+V*P05))))
                  P1 = P10 + V* (P11+V* (P12+V*P13))
                  P2 = P20 + V*P21
                  ZII = P0 + U* (P1+U*P2)
                  IF (IR.EQ.1) THEN
                      ZII1 = ZII
                      WT2 = ((X(1)-X(2))* (XII-X(2))+
     +                      (Y(1)-Y(2))* (YII-Y(2)))**2
                  ELSE
                      ZII2 = ZII
                      WT1 = ((X(2)-X(1))* (XII-X(1))+
     +                      (Y(2)-Y(1))* (YII-Y(1)))**2
                  END IF
  110         CONTINUE
              ZI(IIP) = (WT1*ZII1+WT2*ZII2)/ (WT1+WT2)
              EXTRPI(IIP) = .TRUE.
          END IF
  120 CONTINUE
      END

