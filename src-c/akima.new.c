/* ../src/akima.new.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__6 = 6;
static integer c__10 = 10;
static integer c__3 = 3;
static integer c_n1 = -1;

/* Subroutine */ int sdbi3p_(md, ndp, xd, yd, zd, nip, xi, yi, zi, ier, wk, 
	iwk, extrpi)
integer *md, *ndp;
doublereal *xd, *yd, *zd;
integer *nip;
doublereal *xi, *yi, *zi;
integer *ier;
doublereal *wk;
integer *iwk;
logical *extrpi;
{
    /* Format strings */
    static char fmt_9000[] = "(\002 \002,/,\002*** SDBI3P Error 1: NDP = 9 o\
r less\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,/)";
    static char fmt_9010[] = "(\002 \002,/,\002*** SDBI3P Error 2: NDP not e\
qual to NDPPV\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,\002,  NDPPV \
=\002,i5,/)";
    static char fmt_9020[] = "(\002 \002,/,\002*** SDBI3P Error 3: NIP = 0 o\
r less\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,\002,  NIP =\002,i5,/)";
    static char fmt_9030[] = "(\002    Error detected in SDTRAN called by SD\
BI3P\002,/)";

    /* System generated locals */
    integer wk_dim1, wk_offset, iwk_dim1, iwk_offset, i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer nipi, itli[51], iert, ktli[51], ndppv;
    extern /* Subroutine */ int sdpd3p_();
    static integer nl, nt;
    extern /* Subroutine */ int sdlctn_(), sdtran_(), sdplnl_();
    static integer iip;

    /* Fortran I/O blocks */
    static cilist io___9 = { 0, 6, 0, fmt_9000, 0 };
    static cilist io___10 = { 0, 6, 0, fmt_9010, 0 };
    static cilist io___11 = { 0, 6, 0, fmt_9020, 0 };
    static cilist io___12 = { 0, 6, 0, fmt_9030, 0 };



/* Scattered-data bivariate interpolation */
/* (a master subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine performs bivariate interpolation when the data */
/* points are scattered in the x-y plane.  It is based on the */
/* revised Akima method that has the accuracy of a cubic (third- */
/* degree) polynomial. */

/* The input arguments are */
/*   MD  = mode of computation */
/*       = 1 for new XD-YD (default) */
/*       = 2 for old XD-YD, new ZD */
/*       = 3 for old XD-YD, old ZD, */
/*   NDP = number of data points (must be 10 or greater), */
/*   XD  = array of dimension NDP containing the x coordinates */
/*         of the data points, */
/*   YD  = array of dimension NDP containing the y coordinates */
/*         of the data points, */
/*   ZD  = array of dimension NDP containing the z values at */
/*         the data points, */
/*   NIP = number of output points at which interpolation is */
/*         to be performed (must be 1 or greater), */
/*   XI  = array of dimension NIP containing the x coordinates */
/*         of the output points, */
/*   YI  = array of dimension NIP containing the y coordinates */
/*         of the output points. */

/* The output arguments are */
/*   ZI  = array of dimension NIP, where interpolated z values */
/*         are to be stored, */
/*   IER = error flag */
/*       = 0 for no errors */
/*       = 1 for NDP = 9 or less */
/*       = 2 for NDP not equal to NDPPV */
/*       = 3 for NIP = 0 or less */
/*       = 9 for errors in SDTRAN called by this subroutine. */

/* The other arguments are */
/*   WK  = two-dimensional array of dimension NDP*17 used */
/*         internally as a work area, */
/*   IWK = two-dimensional integer array of dimension NDP*25 */
/*         used internally as a work area. */

/* The very first call to this subroutine and the call with a new */
/* NDP value or new XD and YD arrays must be made with MD=1.  The */
/* call with MD=2 must be preceded by another call with the same */
/* NDP value and same XD and YD arrays.  The call with MD=3 must */
/* be preceded by another call with the same NDP value and same */
/* XD, YD, and ZD arrays.  Between the call with MD=2 and its */
/* preceding call, the IWK array must not be disturbed.  Between */
/* the call with MD=3 and its preceding call, the WK and IWK */
/* arrays must not be disturbed. */

/* The user of this subroutine can save the storage, by NDP*6 */
/* numerical storage units, by placing the statement */
/*     EQUIVALENCE (WK(1,1),IWK(1,20)) */
/* in the program that calls this subroutine. */

/* The constant in the PARAMETER statement below is */
/*   NIPIMX = maximum number of output points to be processed */
/*            at a time. */
/* The constant value has been selected empirically. */

/* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL */
/* subroutines. */


/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Save statement .. */
/*     .. */
/* Error check */
    /* Parameter adjustments */
    iwk_dim1 = *ndp;
    iwk_offset = iwk_dim1 + 1;
    iwk -= iwk_offset;
    wk_dim1 = *ndp;
    wk_offset = wk_dim1 + 1;
    wk -= wk_offset;
    --zd;
    --yd;
    --xd;
    --extrpi;
    --zi;
    --yi;
    --xi;

    /* Function Body */
    if (*ndp <= 9) {
	goto L20;
    }
    if (*md != 2 && *md != 3) {
	ndppv = *ndp;
    } else {
	if (*ndp != ndppv) {
	    goto L30;
	}
    }
    if (*nip <= 0) {
	goto L40;
    }
/* Triangulates the x-y plane.  (for MD=1) */
    if (*md != 2 && *md != 3) {
	sdtran_(ndp, &xd[1], &yd[1], &nt, &iwk[iwk_dim1 + 1], &nl, &iwk[
		iwk_dim1 * 7 + 1], &iert, &iwk[iwk_dim1 + 1], &iwk[iwk_dim1 * 
		7 + 1], &iwk[iwk_dim1 * 13 + 1], &iwk[iwk_dim1 * 14 + 1], &
		iwk[iwk_dim1 * 9 + 1]);
/*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT, */
/*    +                LIST,LPTR,LEND,LTRI,ITL) */
	if (iert > 0) {
	    goto L50;
	}
    }
/* Estimates partial derivatives at all data points.  (for MD=1,2) */
    if (*md != 3) {
	sdpd3p_(ndp, &xd[1], &yd[1], &zd[1], &wk[wk_dim1 + 1], &wk[wk_dim1 * 
		6 + 1], &wk[wk_dim1 * 15 + 1], &wk[wk_dim1 * 17 + 1], &iwk[
		iwk_dim1 * 9 + 1], &iwk[iwk_dim1 * 10 + 1], &iwk[iwk_dim1 * 
		19 + 1]);
/*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP) 
*/
    }
/* Locates all points at which interpolation is to be performed */
/* and interpolates the ZI values.  (for MD=1,2,3) */
    i__1 = *nip;
    for (iip = 1; iip <= i__1; iip += 51) {
/* Computing MIN */
	i__2 = *nip - iip + 1;
	nipi = min(i__2,51);
	sdlctn_(ndp, &xd[1], &yd[1], &nt, &iwk[iwk_dim1 + 1], &nl, &iwk[
		iwk_dim1 * 7 + 1], &nipi, &xi[iip], &yi[iip], ktli, itli);
/*         CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI,ITLI) 
*/
	sdplnl_(ndp, &xd[1], &yd[1], &zd[1], &nt, &iwk[iwk_dim1 + 1], &nl, &
		iwk[iwk_dim1 * 7 + 1], &wk[wk_dim1 + 1], &nipi, &xi[iip], &yi[
		iip], ktli, itli, &zi[iip], &extrpi[iip]);
/*         CALL SDPLNL(NDP,XD,YD,ZD,NT,IPT,NL,IPL,PDD, */
/*    +                NIP,XI,YI,KTLI,ITLI, ZI) */
/* L10: */
    }
/* Normal return */
    *ier = 0;
    return 0;
/* Error exit */
L20:
    s_wsfe(&io___9);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 1;
    return 0;
L30:
    s_wsfe(&io___10);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ndppv, (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 2;
    return 0;
L40:
    s_wsfe(&io___11);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nip), (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 3;
    return 0;
L50:
    s_wsfe(&io___12);
    e_wsfe();
    *ier = 9;
    return 0;
/* Format statement for error message */
} /* sdbi3p_ */

/* Subroutine */ int sdsf3p_(md, ndp, xd, yd, zd, nxi, xi, nyi, yi, zi, ier, 
	wk, iwk, extrpi)
integer *md, *ndp;
doublereal *xd, *yd, *zd;
integer *nxi;
doublereal *xi;
integer *nyi;
doublereal *yi, *zi;
integer *ier;
doublereal *wk;
integer *iwk;
logical *extrpi;
{
    /* Format strings */
    static char fmt_9000[] = "(\002 \002,/,\002*** SDSF3P Error 1: NDP = 9 o\
r less\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,/)";
    static char fmt_9010[] = "(\002 \002,/,\002*** SDSF3P Error 2: NDP not e\
qual to NDPPV\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,\002  NDPPV =\
\002,i5,/)";
    static char fmt_9020[] = "(\002 \002,/,\002*** SDSF3P Error 3: NXI = 0 o\
r less\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,\002  NXI =\002,i5,\002\
,  NYI =\002,i5,/)";
    static char fmt_9030[] = "(\002 \002,/,\002*** SDSF3P Error 4: NYI = 0 o\
r less\002,/,\002    MD =\002,i5,\002,  NDP =\002,i5,\002  NXI =\002,i5,\002\
,  NYI =\002,i5,/)";
    static char fmt_9040[] = "(\002    Error detected in SDTRAN called by SD\
SF3P\002,/)";

    /* System generated locals */
    integer wk_dim1, wk_offset, zi_dim1, zi_offset, iwk_dim1, iwk_offset, 
	    extrpi_dim1, extrpi_offset, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer nipi, itli[51], iert, ktli[51], ndppv;
    extern /* Subroutine */ int sdpd3p_();
    static integer nl, nt;
    extern /* Subroutine */ int sdlctn_(), sdtran_(), sdplnl_();
    static integer iip, ixi, iyi;
    static doublereal yii[51];

    /* Fortran I/O blocks */
    static cilist io___24 = { 0, 6, 0, fmt_9000, 0 };
    static cilist io___25 = { 0, 6, 0, fmt_9010, 0 };
    static cilist io___26 = { 0, 6, 0, fmt_9020, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_9030, 0 };
    static cilist io___28 = { 0, 6, 0, fmt_9040, 0 };



/* Scattered-data smooth surface fitting */
/* (a master subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine performs smooth surface fitting when the data */
/* points are scattered in the x-y plane.  It is based on the */
/* revised Akima method that has the accuracy of a cubic (third- */
/* degree) polynomial. */

/* The input arguments are */
/*   MD  = mode of computation */
/*       = 1 for new XD-YD (default) */
/*       = 2 for old XD-YD, new ZD */
/*       = 3 for old XD-YD, old ZD, */
/*   NDP = number of data points (must be 10 or greater), */
/*   XD  = array of dimension NDP containing the x coordinates */
/*         of the data points, */
/*   YD  = array of dimension NDP containing the y coordinates */
/*         of the data points, */
/*   ZD  = array of dimension NDP containing the z values at */
/*         the data points, */
/*   NXI = number of output grid points in the x coordinate */
/*         (must be 1 or greater), */
/*   XI  = array of dimension NXI containing the x coordinates */
/*         of the output grid points, */
/*   NYI = number of output grid points in the y coordinate */
/*         (must be 1 or greater), */
/*   YI  = array of dimension NYI containing the y coordinates */
/*         of the output grid points. */

/* The output arguments are */
/*   ZI  = two-dimensional array of dimension NXI*NYI, where */
/*         the interpolated z values at the output grid points */
/*         are to be stored, */
/*   IER = error flag */
/*       = 0 for no errors */
/*       = 1 for NDP = 9 or less */
/*       = 2 for NDP not equal to NDPPV */
/*       = 3 for NXI = 0 or less */
/*       = 4 for NYI = 0 or less */
/*       = 9 for errors in SDTRAN called by this subroutine. */

/* The other arguments are */
/*   WK  = two-dimensional array of dimension NDP*36 used */
/*         internally as a work area, */
/*   IWK = two-dimensional integer array of dimension NDP*25 */
/*         used internally as a work area. */

/* The very first call to this subroutine and the call with a new */
/* NDP value or new XD and YD arrays must be made with MD=1.  The */
/* call with MD=2 must be preceded by another call with the same */
/* NDP value and same XD and YD arrays.  The call with MD=3 must */
/* be preceded by another call with the same NDP value and same */
/* XD, YD, and ZD arrays.  Between the call with MD=2 and its */
/* preceding call, the IWK array must not be disturbed.  Between */
/* the call with MD=3 and its preceding call, the WK and IWK */
/* arrays must not be disturbed. */

/* The user of this subroutine can save the storage, by NDP*6 */
/* numeric storage units, by placing the statement */
/*     EQUIVALENCE (WK(1,1),IWK(1,20)) */
/* in the program that calls this subroutine. */

/* The constant in the PARAMETER statement below is */
/*   NIPIMX = maximum number of output points to be processed */
/*            at a time. */
/* The constant value has been selected empirically. */

/* This subroutine calls the SDTRAN, SDPD3P, SDLCTN, and SDPLNL */
/* subroutines. */


/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Save statement .. */
/*     .. */
/* Error check */
    /* Parameter adjustments */
    iwk_dim1 = *ndp;
    iwk_offset = iwk_dim1 + 1;
    iwk -= iwk_offset;
    wk_dim1 = *ndp;
    wk_offset = wk_dim1 + 1;
    wk -= wk_offset;
    --zd;
    --yd;
    --xd;
    --xi;
    extrpi_dim1 = *nxi;
    extrpi_offset = extrpi_dim1 + 1;
    extrpi -= extrpi_offset;
    zi_dim1 = *nxi;
    zi_offset = zi_dim1 + 1;
    zi -= zi_offset;
    --yi;

    /* Function Body */
    if (*ndp <= 9) {
	goto L40;
    }
    if (*md != 2 && *md != 3) {
	ndppv = *ndp;
    } else {
	if (*ndp != ndppv) {
	    goto L50;
	}
    }
    if (*nxi <= 0) {
	goto L60;
    }
    if (*nyi <= 0) {
	goto L70;
    }
/* Triangulates the x-y plane.  (for MD=1) */
    if (*md != 2 && *md != 3) {
	sdtran_(ndp, &xd[1], &yd[1], &nt, &iwk[iwk_dim1 + 1], &nl, &iwk[
		iwk_dim1 * 7 + 1], &iert, &iwk[iwk_dim1 + 1], &iwk[iwk_dim1 * 
		7 + 1], &iwk[iwk_dim1 * 13 + 1], &iwk[iwk_dim1 * 14 + 1], &
		iwk[iwk_dim1 * 9 + 1]);
/*         CALL SDTRAN(NDP,XD,YD, NT,IPT,NL,IPL,IERT, */
/*    +                LIST,LPTR,LEND,LTRI,ITL) */
	if (iert > 0) {
	    goto L80;
	}
    }
/* Estimates partial derivatives at all data points.  (for MD=1,2) */
    if (*md != 3) {
	sdpd3p_(ndp, &xd[1], &yd[1], &zd[1], &wk[wk_dim1 + 1], &wk[wk_dim1 * 
		6 + 1], &wk[wk_dim1 * 15 + 1], &wk[wk_dim1 * 17 + 1], &iwk[
		iwk_dim1 * 9 + 1], &iwk[iwk_dim1 * 10 + 1], &iwk[iwk_dim1 * 
		19 + 1]);
/*         CALL SDPD3P(NDP,XD,YD,ZD, PDD, CF3,CFL1,DSQ,IDSQ,IPC,NCP) 
*/
    }
/* Locates all grid points at which interpolation is to be */
/* performed and interpolates the ZI values.  (for MD=1,2,3) */
    i__1 = *nyi;
    for (iyi = 1; iyi <= i__1; ++iyi) {
	for (iip = 1; iip <= 51; ++iip) {
	    yii[iip - 1] = yi[iyi];
/* L10: */
	}
	i__2 = *nxi;
	for (ixi = 1; ixi <= i__2; ixi += 51) {
/* Computing MIN */
	    i__3 = *nxi - ixi + 1;
	    nipi = min(i__3,51);
	    sdlctn_(ndp, &xd[1], &yd[1], &nt, &iwk[iwk_dim1 + 1], &nl, &iwk[
		    iwk_dim1 * 7 + 1], &nipi, &xi[ixi], yii, ktli, itli);
/*             CALL SDLCTN(NDP,XD,YD,NT,IPT,NL,IPL,NIP,XI,YI, KTLI
,ITLI) */
	    sdplnl_(ndp, &xd[1], &yd[1], &zd[1], &nt, &iwk[iwk_dim1 + 1], &nl,
		     &iwk[iwk_dim1 * 7 + 1], &wk[wk_dim1 + 1], &nipi, &xi[ixi]
		    , yii, ktli, itli, &zi[ixi + iyi * zi_dim1], &extrpi[ixi 
		    + iyi * extrpi_dim1]);
/*             CALL SDPLNL(NDP,XD,YD,ZD,NT,ITP,NL,IPL,PDD, */
/*    +                    NIP,XI,YI,KTLI,ITLI, ZI) */
/* L20: */
	}
/* L30: */
    }
/* Normal return */
    *ier = 0;
    return 0;
/* Error exit */
L40:
    s_wsfe(&io___24);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 1;
    return 0;
L50:
    s_wsfe(&io___25);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ndppv, (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 2;
    return 0;
L60:
    s_wsfe(&io___26);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nxi), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nyi), (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 3;
    return 0;
L70:
    s_wsfe(&io___27);
    do_fio(&c__1, (char *)&(*md), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nxi), (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&(*nyi), (ftnlen)sizeof(integer));
    e_wsfe();
    *ier = 4;
    return 0;
L80:
    s_wsfe(&io___28);
    e_wsfe();
    *ier = 9;
    return 0;
/* Format statement for error message */
} /* sdsf3p_ */

/* Subroutine */ int sdtran_(ndp, xd, yd, nt, ipt, nl, ipl, iert, list, lptr, 
	lend, ltri, itl)
integer *ndp;
doublereal *xd, *yd;
integer *nt, *ipt, *nl, *ipl, *iert, *list, *lptr, *lend, *ltri, *itl;
{
    /* Format strings */
    static char fmt_9000[] = "(\002 \002,/,\002*** SDTRAN Error 1: NDP = 3 o\
r less\002,/,\002    NDP =\002,i5)";
    static char fmt_9010[] = "(\002 \002,/,\002*** SDTRAN Error 2: \002,\002\
The first three data points are collinear.\002,/)";
    static char fmt_9020[] = "(\002 \002,/,\002*** SDTRAN Error 3: Identical\
 data points\002,/,\002    NDP =\002,i5,\002,  IP1 =\002,i5,\002,  XD =\002,\
e11.3,\002,  YD =\002,e11.3)";
    static char fmt_9030[] = "(\002 \002,/,\002*** SDTRAN Error 4: NDP outsi\
de its valid\002,\002 range\002,/,\002    NDP =\002,i5)";
    static char fmt_9040[] = "(\002 \002,/,\002*** SDTRAN Error 5: \002,\002\
Invalid data structure (LIST,LPTR,LEND)\002,/)";

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer iertl, iertm;
    extern /* Subroutine */ int sdtrch_();
    static integer ip1;
    extern /* Subroutine */ int sdtrtt_();

    /* Fortran I/O blocks */
    static cilist io___31 = { 0, 6, 0, fmt_9000, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_9010, 0 };
    static cilist io___34 = { 0, 6, 0, fmt_9020, 0 };
    static cilist io___35 = { 0, 6, 0, fmt_9030, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_9040, 0 };



/* Triangulation of the data area in a plane with a scattered data */
/* point set */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine triangulates the data area in the x-y plane with */
/* a scattered data point set.  It divides the data area into a */
/* number of triangles and determines line segments that form the */
/* border of the data area. */

/* This subroutine consists of the following two steps, i.e., */
/* (1) basic triangulation in the convex hull of the data points, */
/* and (2) removal of thin triangles along the border line of the */
/* data area.  It calls the SDTRCH and SDTRTT subroutines, that */
/* correspond to Steps (1) and (2), respectively. */

/* The input arguments are */
/*   NDP  = number of data points (must be greater than 3), */
/*   XD   = array of dimension NDP containing the x */
/*          coordinates of the data points, */
/*   YD   = array of dimension NDP containing the y */
/*          coordinates of the data points. */

/* The output arguments are */
/*   NT   = number of triangles (its maximum is 2*NDP-5), */
/*   IPT  = two-dimensional integer array of dimension */
/*          (3,NT), where the point numbers of the vertexes */
/*          of the ITth triangle are to be stored counter- */
/*          clockwise in the ITth column, where IT = 1, 2, */
/*          ..., NT, */
/*   NL   = number of border line segments (its maximum is */
/*          NDP), */
/*   IPL  = two-dimensional integer array of dimension */
/*          (2,NL), where the point numbers of the end */
/*          points of the (IL)th border line segment are to */
/*          be stored counterclockwise in the ILth column, */
/*          where IL = 1, 2, ..., NL, with the line segments */
/*          stored counterclockwise, */
/*   IERT = error flag */
/*        = 0 for no errors */
/*        = 1 for NDP = 3 or less */
/*        = 2 for identical data points */
/*        = 3 for all collinear data points. */

/* The other arguments are */
/*   LIST = integer array of dimension 6*NDP USED internally */
/*          as a work area, */
/*   LPTR = integer array of dimension 6*NDP USED internally */
/*          as a work area, */
/*   LEND = integer array of dimension NDP USED internally as */
/*          a work area, */
/*   LTRI = two-dimensional integer array of dimension 12*NDP */
/*          used internally as a work area. */
/*   ITL  = integer array of dimension NDP used internally as */
/*          a work area. */


/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/* Basic triangulation */
    /* Parameter adjustments */
    --itl;
    ltri -= 13;
    --lend;
    lptr -= 7;
    list -= 7;
    --yd;
    --xd;
    ipt -= 4;
    ipl -= 3;

    /* Function Body */
    sdtrch_(ndp, &xd[1], &yd[1], nt, &ipt[4], nl, &ipl[3], &iertm, &iertl, &
	    list[7], &lptr[7], &lend[1], &ltri[13]);
    if (iertm != 0) {
	goto L10;
    }
    if (iertl != 0) {
	goto L20;
    }
    *iert = 0;
/* Removal of thin triangles that share border line segments */
    sdtrtt_(ndp, &xd[1], &yd[1], nt, &ipt[4], nl, &ipl[3], &itl[1]);
    return 0;
/* Error exit */
L10:
    if (iertm == -1) {
	*iert = 1;
	s_wsfe(&io___31);
	do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (iertm == -2) {
	*iert = 2;
	s_wsfe(&io___32);
	e_wsfe();
    } else {
	*iert = 3;
	ip1 = iertm;
	s_wsfe(&io___34);
	do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&ip1, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&xd[ip1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yd[ip1], (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    return 0;
L20:
    if (iertl == 1) {
	*iert = 4;
	s_wsfe(&io___35);
	do_fio(&c__1, (char *)&(*ndp), (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (iertl == 2) {
	*iert = 5;
	s_wsfe(&io___36);
	e_wsfe();
    }
    return 0;
/* Format statements */
} /* sdtran_ */

/* Subroutine */ int sdtrch_(ndp, xd, yd, nt, ipt, nl, ipl, iertm, iertl, 
	list, lptr, lend, ltri)
integer *ndp;
doublereal *xd, *yd;
integer *nt, *ipt, *nl, *ipl, *iertm, *iertl, *list, *lptr, *lend, *ltri;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer ipl11, ipl21, lnew, i, j, i1, i2, il;
    extern /* Subroutine */ int trmesh_();
    static integer il1, il2;
    extern /* Subroutine */ int trlist_();
    static integer lcc[1], lct[1];


/* Basic triangulation in the convex hull of a scattered data point */
/* set in a plane */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine triangulates the data area that is a convex hull */
/* of the scattered data points in the x-y plane.  It divides the */
/* data area into a number of triangles and determines line segments */
/* that form the border of the data area. */

/* This subroutine depends on the TRIPACK package of ACM Algorithm */
/* 751 by R. J. Renka.  It calls the TRMESH and TRLIST subroutines */
/* included in the package.  The TRMESH subroutine in turn calls */
/* either directly or indirectly 12 other subprograms included in */
/* the package. */

/* The input arguments are */
/*   NDP   = number of data points (must be greater than 3), */
/*   XD    = array of dimension NDP containing the x */
/*           coordinates of the data points, */
/*   YD    = array of dimension NDP containing the y */
/*           coordinates of the data points. */

/* The output arguments are */
/*   NT    = number of triangles (its maximum is 2*NDP-5), */
/*   IPT   = two-dimensional integer array of dimension */
/*           (3,NT), where the point numbers of the vertexes */
/*           of the ITth triangle are to be stored counter- */
/*           clockwise in the ITth column, where IT = 1, 2, */
/*           ..., NT, */
/*   NL    = number of border line segments (its maximum is */
/*           NDP), */
/*   IPL   = two-dimensional integer array of dimension */
/*           (2,NL), where the point numbers of the end */
/*           points of the (IL)th border line segment are to */
/*           be stored counterclockwise in the ILth column, */
/*           where IL = 1, 2, ..., NL, with the line segments */
/*           stored counterclockwise, */
/*   IERTM = error flag from the TRMESH subroutine, */
/*         =  0 for no errors */
/*         = -1 for NDP = 3 or less */
/*         = -2 for the first three collinear data points, */
/*         =  L for the Lth data point identical to some */
/*            Mth data point, M > L. */
/*   IERTL = error flag from the TRLIST subroutine, */
/*         = 0 for no errors */
/*         = 1 for invalid NCC, NDP, or NROW value. */
/*         = 2 for invalid data structure (LIST,LPTR,LEND). */

/* The other arguments are */
/*   LIST  = integer array of dimension 6*NDP USED internally */
/*           as a work area, */
/*   LPTR  = integer array of dimension 6*NDP USED internally */
/*           as a work area, */
/*   LEND  = integer array of dimension NDP USED internally as */
/*           a work area, */
/*   LTRI  = two-dimensional integer array of dimension 12*NDP */
/*           used internally as a work area. */


/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* Performs basic triangulation. */
    /* Parameter adjustments */
    --lend;
    --yd;
    --xd;
    ipt -= 4;
    ipl -= 3;
    --list;
    --lptr;
    ltri -= 7;

    /* Function Body */
    trmesh_(ndp, &xd[1], &yd[1], &list[1], &lptr[1], &lend[1], &lnew, iertm);
    if (*iertm != 0) {
	return 0;
    }
    trlist_(&c__0, lcc, ndp, &list[1], &lptr[1], &lend[1], &c__6, nt, &ltri[7]
	    , lct, iertl);
    if (*iertl != 0) {
	return 0;
    }
/* Extracts the triangle data from the LTRI array and set the IPT */
/* array. */
    i__1 = *nt;
    for (j = 1; j <= i__1; ++j) {
	for (i = 1; i <= 3; ++i) {
	    ipt[i + j * 3] = ltri[i + j * 6];
/* L10: */
	}
/* L20: */
    }
/* Extracts the border-line-segment data from the LTRI array and */
/* set the IPL array. */
    il = 0;
    i__1 = *nt;
    for (j = 1; j <= i__1; ++j) {
	for (i = 1; i <= 3; ++i) {
	    if (ltri[i + 3 + j * 6] <= 0) {
		goto L40;
	    }
/* L30: */
	}
	goto L50;
L40:
	++il;
	i1 = i % 3 + 1;
	i2 = (i + 1) % 3 + 1;
	ipl[(il << 1) + 1] = ltri[i1 + j * 6];
	ipl[(il << 1) + 2] = ltri[i2 + j * 6];
L50:
	;
    }
    *nl = il;
/* Sorts the IPL array. */
    i__1 = *nl - 1;
    for (il1 = 1; il1 <= i__1; ++il1) {
	i__2 = *nl;
	for (il2 = il1 + 1; il2 <= i__2; ++il2) {
	    if (ipl[(il2 << 1) + 1] == ipl[(il1 << 1) + 2]) {
		goto L70;
	    }
/* L60: */
	}
L70:
	ipl11 = ipl[(il1 + 1 << 1) + 1];
	ipl21 = ipl[(il1 + 1 << 1) + 2];
	ipl[(il1 + 1 << 1) + 1] = ipl[(il2 << 1) + 1];
	ipl[(il1 + 1 << 1) + 2] = ipl[(il2 << 1) + 2];
	ipl[(il2 << 1) + 1] = ipl11;
	ipl[(il2 << 1) + 2] = ipl21;
/* L80: */
    }
    return 0;
} /* sdtrch_ */

/* Subroutine */ int sdtrtt_(ndp, xd, yd, nt, ipt, nl, ipl, itl)
integer *ndp;
doublereal *xd, *yd;
integer *nt, *ipt, *nl, *ipl, *itl;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static integer irep, modif, il, it, iv, il0, il1, ip1, ip2, ip3, it0, nl0,
	     il00;
    static doublereal hbr;
    static integer ilp1, ipl1, ilr1, ipl2, itp1, ivp1;


/* Removal of thin triangles along the border line of triangulation */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine removes thin triangles along the border line of */
/* triangulation. */

/* The input arguments are */
/*   NDP = number of data points (must be greater than 3), */
/*   XD  = array of dimension NDP containing the x */
/*         coordinates of the data points, */
/*   YD  = array of dimension NDP containing the y */
/*         coordinates of the data points. */

/* The input and output arguments are */
/*   NT  = number of triangles (its maximum is 2*NDP-5), */
/*   IPT = two-dimensional integer array of dimension */
/*         (3,NT), where the point numbers of the vertexes */
/*         of the ITth triangle are to be stored counter- */
/*         clockwise in the ITth column, where IT = 1, 2, */
/*         ..., NT, */
/*   NL  = number of border line segments (its maximum is */
/*         NDP), */
/*   IPL = two-dimensional integer array of dimension */
/*         (2,NL), where the point numbers of the end */
/*         points of the (IL)th border line segment are to */
/*         be stored counterclockwise in the ILth column, */
/*         where IL = 1, 2, ..., NL, with the line segments */
/*         stored counterclockwise. */

/* The other argument is */
/*   ITL = integer array of dimension NDP used internally as */
/*         a work area. */

/* The constants in the PARAMETER statement below are */
/*   HBRMN = minimum value of the height-to-bottom ratio of a */
/*           triangle along the border line of the data area, */
/*   NRRTT = number of repetitions in thin triangle removal. */
/* The constant values have been selected empirically. */

/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Statement Functions .. */
/*     .. */
/* Statement Function definitions */
/*     .. */
/* Triangle numbers of triangles that share line segments with the */
/* border line. */
    /* Parameter adjustments */
    --itl;
    --yd;
    --xd;
    ipt -= 4;
    ipl -= 3;

    /* Function Body */
    i__1 = *nl;
    for (il = 1; il <= i__1; ++il) {
	ipl1 = ipl[(il << 1) + 1];
	ipl2 = ipl[(il << 1) + 2];
	i__2 = *nt;
	for (it = 1; it <= i__2; ++it) {
	    if (ipl1 == ipt[it * 3 + 1] || ipl1 == ipt[it * 3 + 2] || ipl1 == 
		    ipt[it * 3 + 3]) {
		if (ipl2 == ipt[it * 3 + 1] || ipl2 == ipt[it * 3 + 2] || 
			ipl2 == ipt[it * 3 + 3]) {
		    itl[il] = it;
		    goto L20;
		}
	    }
/* L10: */
	}
L20:
	;
    }
/* Removes thin triangles that share line segments with the border */
/* line. */
    for (irep = 1; irep <= 5; ++irep) {
	modif = 0;
	nl0 = *nl;
	il = 0;
	i__1 = nl0;
	for (il0 = 1; il0 <= i__1; ++il0) {
	    ++il;
	    ip1 = ipl[(il << 1) + 1];
	    ip2 = ipl[(il << 1) + 2];
	    it = itl[il];
/* Calculates the height-to-bottom ratio of the triangle. */
	    if (ipt[it * 3 + 1] != ip1 && ipt[it * 3 + 1] != ip2) {
		ip3 = ipt[it * 3 + 1];
	    } else if (ipt[it * 3 + 2] != ip1 && ipt[it * 3 + 2] != ip2) {
		ip3 = ipt[it * 3 + 2];
	    } else {
		ip3 = ipt[it * 3 + 3];
	    }
/* Computing 2nd power */
	    d__1 = xd[ip2] - xd[ip1];
/* Computing 2nd power */
	    d__2 = yd[ip2] - yd[ip1];
	    hbr = ((yd[ip3] - yd[ip1]) * (xd[ip2] - xd[ip1]) - (xd[ip3] - xd[
		    ip1]) * (yd[ip2] - yd[ip1])) / (d__1 * d__1 + d__2 * d__2)
		    ;
	    if (hbr < .1) {
		modif = 1;
/* Removes this triangle when applicable. */
		itp1 = it + 1;
		i__2 = *nt;
		for (it0 = itp1; it0 <= i__2; ++it0) {
		    ipt[(it0 - 1) * 3 + 1] = ipt[it0 * 3 + 1];
		    ipt[(it0 - 1) * 3 + 2] = ipt[it0 * 3 + 2];
		    ipt[(it0 - 1) * 3 + 3] = ipt[it0 * 3 + 3];
/* L30: */
		}
		--(*nt);
		i__2 = *nl;
		for (il00 = 1; il00 <= i__2; ++il00) {
		    if (itl[il00] > it) {
			--itl[il00];
		    }
/* L40: */
		}
/* Replaces the border line segment with two new line segments
. */
		if (il < *nl) {
		    ilp1 = il + 1;
		    i__2 = *nl;
		    for (ilr1 = ilp1; ilr1 <= i__2; ++ilr1) {
			il1 = *nl + ilp1 - ilr1;
			ipl[(il1 + 1 << 1) + 1] = ipl[(il1 << 1) + 1];
			ipl[(il1 + 1 << 1) + 2] = ipl[(il1 << 1) + 2];
			itl[il1 + 1] = itl[il1];
/* L50: */
		    }
		}
/* - Adds the first new line segment. */
		ipl[(il << 1) + 1] = ip1;
		ipl[(il << 1) + 2] = ip3;
		i__2 = *nt;
		for (it0 = 1; it0 <= i__2; ++it0) {
		    for (iv = 1; iv <= 3; ++iv) {
			if (ipt[iv + it0 * 3] == ip1 || ipt[iv + it0 * 3] == 
				ip3) {
			    ivp1 = iv % 3 + 1;
			    if (ipt[ivp1 + it0 * 3] == ip1 || ipt[ivp1 + it0 *
				     3] == ip3) {
				goto L80;
			    }
			}
/* L60: */
		    }
/* L70: */
		}
L80:
		itl[il] = it0;
/* - Adds the second new line segment. */
		++il;
		ipl[(il << 1) + 1] = ip3;
		ipl[(il << 1) + 2] = ip2;
		i__2 = *nt;
		for (it0 = 1; it0 <= i__2; ++it0) {
		    for (iv = 1; iv <= 3; ++iv) {
			if (ipt[iv + it0 * 3] == ip3 || ipt[iv + it0 * 3] == 
				ip2) {
			    ivp1 = iv % 3 + 1;
			    if (ipt[ivp1 + it0 * 3] == ip3 || ipt[ivp1 + it0 *
				     3] == ip2) {
				goto L110;
			    }
			}
/* L90: */
		    }
/* L100: */
		}
L110:
		itl[il] = it0;
		++(*nl);
	    }
/* L120: */
	}
	if (modif == 0) {
	    return 0;
	}
/* L130: */
    }
    return 0;
} /* sdtrtt_ */

/* Subroutine */ int sdpd3p_(ndp, xd, yd, zd, pdd, cf3, cfl1, dsq, idsq, ipc, 
	ncp)
integer *ndp;
doublereal *xd, *yd, *zd, *pdd, *cf3, *cfl1, *dsq;
integer *idsq, *ipc, *ncp;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    double exp();

    /* Local variables */
    static doublereal anpe;
    static integer idpi;
    static doublereal pdpe[125]	/* was [5][25] */, rvwt[25];
    static integer j, k;
    static doublereal pddif[5], x, y, pddii[5];
    static integer ipcpe[250]	/* was [10][25] */, idppe[25], ncp2p1;
    static doublereal alpwt;
    static integer j1, j2;
    static doublereal smwtf, smwti, anpem1;
    static integer idppe1;
    extern /* Subroutine */ int sdcf3p_();
    static doublereal a01, a02, a03, a10, a11, a12, a20, a21, a30;
    extern /* Subroutine */ int sdls1p_();
    static integer jj;
    static doublereal ampdpe[5];
    extern /* Subroutine */ int sdcldp_();
    static doublereal zx, zy, sspdpe[5];
    static integer ipe, npe, imn, jmn;
    static doublereal wtf, wti, pwt[25];
    static integer idp1, idp2, ipe1, ncp2;


/* Partial derivatives for bivariate interpolation and surface */
/* fitting for scattered data */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine estimates partial derivatives of the first and */
/* second orders at the data points for bivariate interpolation */
/* and surface fitting for scattered data.  In most cases, this */
/* subroutine has the accuracy of a cubic (third-degree) */
/* polynomial. */

/* The input arguments are */
/*   NDP  = number of data points, */
/*   XD   = array of dimension NDP containing the x */
/*          coordinates of the data points, */
/*   YD   = array of dimension NDP containing the y */
/*          coordinates of the data points, */
/*   ZD   = array of dimension NDP containing the z values */
/*          at the data points. */

/* The output argument is */
/*   PDD  = two-dimensional array of dimension 5*NDP, where */
/*          the estimated zx, zy, zxx, zxy, and zyy values */
/*          at the IDPth data point are to be stored in the */
/*          IDPth row, where IDP = 1, 2, ..., NDP. */

/* The other arguments are */
/*   CF3  = two-dimensional array of dimension 9*NDP used */
/*          internally as a work area, */
/*   CFL1 = two-dimensional array of dimension 2*NDP used */
/*          internally as a work area, */
/*   DSQ  = array of dimension NDP used internally as a work */
/*          area, */
/*   IDSQ = integer array of dimension NDP used internally */
/*          as a work area, */
/*   IPC  = two-dimensional integer array of dimension 9*NDP */
/*          used internally as a work area, */
/*   NCP  = integer array of dimension NDP used internally */
/*          as a work area. */

/* The constant in the first PARAMETER statement below is */
/*   NPEMX = maximum number of primary estimates. */
/* The constant value has been selected empirically. */

/* The constants in the second PARAMETER statement below are */
/*   NPEAMN = minimum number of primary estimates, */
/*   NPEAMX = maximum number of primary estimates when */
/*            additional primary estimates are added. */
/* The constant values have been selected empirically. */

/* This subroutine calls the SDCLDP, SDCF3P, and SDLS1P */
/* subroutines. */


/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* Calculation */
/* Selects, at each of the data points, nine data points closest */
/* to the data point in question. */
    /* Parameter adjustments */
    --ncp;
    ipc -= 10;
    --idsq;
    --dsq;
    cfl1 -= 3;
    cf3 -= 10;
    pdd -= 6;
    --zd;
    --yd;
    --xd;

    /* Function Body */
    sdcldp_(ndp, &xd[1], &yd[1], &ipc[10], &dsq[1], &idsq[1]);
/* Fits, at each of the data points, a cubic (third-degree) */
/* polynomial to z values at the 10 data points that consist of */
/* the data point in question and 9 data points closest to it. */
    sdcf3p_(ndp, &xd[1], &yd[1], &zd[1], &ipc[10], &cf3[10], &ncp[1]);
/* Performs, at each of the data points, the least-squares fit of */
/* a plane to z values at the 10 data points. */
    sdls1p_(ndp, &xd[1], &yd[1], &zd[1], &ipc[10], &ncp[1], &cfl1[3]);
/* Outermost DO-loop with respect to the data point */
    i__1 = *ndp;
    for (idp1 = 1; idp1 <= i__1; ++idp1) {
/* Selects data point sets for sets of primary estimates of partial */
/* derivatives. */
/* - Selects a candidate. */
	npe = 0;
	i__2 = *ndp;
	for (idp2 = 1; idp2 <= i__2; ++idp2) {
	    ncp2 = ncp[idp2];
	    ncp2p1 = ncp2 + 1;
	    if (idp2 == idp1) {
		goto L20;
	    }
	    i__3 = ncp2;
	    for (j = 1; j <= i__3; ++j) {
		if (ipc[j + idp2 * 9] == idp1) {
		    goto L20;
		}
/* L10: */
	    }
	    goto L80;
L20:
	    ipcpe[(npe + 1) * 10 - 10] = idp2;
	    i__3 = ncp2;
	    for (j = 1; j <= i__3; ++j) {
		ipcpe[j + 1 + (npe + 1) * 10 - 11] = ipc[j + idp2 * 9];
/* L30: */
	    }
	    i__3 = ncp2;
	    for (j1 = 1; j1 <= i__3; ++j1) {
		jmn = j1;
		imn = ipcpe[jmn + (npe + 1) * 10 - 11];
		i__4 = ncp2p1;
		for (j2 = j1; j2 <= i__4; ++j2) {
		    if (ipcpe[j2 + (npe + 1) * 10 - 11] < imn) {
			jmn = j2;
			imn = ipcpe[jmn + (npe + 1) * 10 - 11];
		    }
/* L40: */
		}
		ipcpe[jmn + (npe + 1) * 10 - 11] = ipcpe[j1 + (npe + 1) * 10 
			- 11];
		ipcpe[j1 + (npe + 1) * 10 - 11] = imn;
/* L50: */
	    }
/* - Checks whether or not the candidate has already been included
. */
	    if (npe > 0) {
		i__3 = npe;
		for (ipe1 = 1; ipe1 <= i__3; ++ipe1) {
		    idppe1 = idppe[ipe1 - 1];
		    if (ncp2 != ncp[idppe1]) {
			goto L70;
		    }
		    i__4 = ncp2p1;
		    for (j = 1; j <= i__4; ++j) {
			if (ipcpe[j + (npe + 1) * 10 - 11] != ipcpe[j + ipe1 *
				 10 - 11]) {
			    goto L70;
			}
/* L60: */
		    }
		    goto L80;
L70:
		    ;
		}
	    }
	    ++npe;
	    idppe[npe - 1] = idp2;
	    if (npe >= 25) {
		goto L90;
	    }
L80:
	    ;
	}
L90:
/* Adds additional closest data points when necessary. */
	if (npe < 3) {
	    for (jj = 1; jj <= 9; ++jj) {
		idp2 = ipc[jj + idp1 * 9];
		ncp2 = ncp[idp2];
		ncp2p1 = ncp2 + 1;
		ipcpe[(npe + 1) * 10 - 10] = idp2;
		i__2 = ncp2;
		for (j = 1; j <= i__2; ++j) {
		    ipcpe[j + 1 + (npe + 1) * 10 - 11] = ipc[j + idp2 * 9];
/* L100: */
		}
		i__2 = ncp2;
		for (j1 = 1; j1 <= i__2; ++j1) {
		    jmn = j1;
		    imn = ipcpe[jmn + (npe + 1) * 10 - 11];
		    i__3 = ncp2p1;
		    for (j2 = j1; j2 <= i__3; ++j2) {
			if (ipcpe[j2 + (npe + 1) * 10 - 11] < imn) {
			    jmn = j2;
			    imn = ipcpe[jmn + (npe + 1) * 10 - 11];
			}
/* L110: */
		    }
		    ipcpe[jmn + (npe + 1) * 10 - 11] = ipcpe[j1 + (npe + 1) * 
			    10 - 11];
		    ipcpe[j1 + (npe + 1) * 10 - 11] = imn;
/* L120: */
		}
		if (npe > 0) {
		    i__2 = npe;
		    for (ipe1 = 1; ipe1 <= i__2; ++ipe1) {
			idppe1 = idppe[ipe1 - 1];
			if (ncp2 != ncp[idppe1]) {
			    goto L140;
			}
			i__3 = ncp2p1;
			for (j = 1; j <= i__3; ++j) {
			    if (ipcpe[j + (npe + 1) * 10 - 11] != ipcpe[j + 
				    ipe1 * 10 - 11]) {
				goto L140;
			    }
/* L130: */
			}
			goto L150;
L140:
			;
		    }
		}
		++npe;
		idppe[npe - 1] = idp2;
		if (npe >= 6) {
		    goto L160;
		}
L150:
		;
	    }
	}
L160:
/* Calculates the primary estimates of partial derivatives. */
	x = xd[idp1];
	y = yd[idp1];
	i__2 = npe;
	for (ipe = 1; ipe <= i__2; ++ipe) {
	    idpi = idppe[ipe - 1];
	    a10 = cf3[idpi * 9 + 1];
	    a20 = cf3[idpi * 9 + 2];
	    a30 = cf3[idpi * 9 + 3];
	    a01 = cf3[idpi * 9 + 4];
	    a11 = cf3[idpi * 9 + 5];
	    a21 = cf3[idpi * 9 + 6];
	    a02 = cf3[idpi * 9 + 7];
	    a12 = cf3[idpi * 9 + 8];
	    a03 = cf3[idpi * 9 + 9];
	    pdpe[ipe * 5 - 5] = a10 + x * (a20 * (float)2. + x * (float)3. * 
		    a30) + y * (a11 + a21 * (float)2. * x + a12 * y);
	    pdpe[ipe * 5 - 4] = a01 + y * (a02 * (float)2. + y * (float)3. * 
		    a03) + x * (a11 + a12 * (float)2. * y + a21 * x);
	    pdpe[ipe * 5 - 3] = a20 * (float)2. + a30 * (float)6. * x + a21 * 
		    (float)2. * y;
	    pdpe[ipe * 5 - 2] = a11 + a21 * (float)2. * x + a12 * (float)2. * 
		    y;
	    pdpe[ipe * 5 - 1] = a02 * (float)2. + a03 * (float)6. * y + a12 * 
		    (float)2. * x;
/* L170: */
	}
	if (npe == 1) {
	    goto L290;
	}
/* Weighted values of partial derivatives (through the statement */
/* labeled 280 + 1) */
/* Calculates the probability weight. */
	anpe = (doublereal) npe;
	anpem1 = (doublereal) (npe - 1);
	for (k = 1; k <= 5; ++k) {
	    ampdpe[k - 1] = (float)0.;
	    sspdpe[k - 1] = (float)0.;
	    i__2 = npe;
	    for (ipe = 1; ipe <= i__2; ++ipe) {
		ampdpe[k - 1] += pdpe[k + ipe * 5 - 6];
/* Computing 2nd power */
		d__1 = pdpe[k + ipe * 5 - 6];
		sspdpe[k - 1] += d__1 * d__1;
/* L180: */
	    }
	    ampdpe[k - 1] /= anpe;
/* Computing 2nd power */
	    d__1 = ampdpe[k - 1];
	    sspdpe[k - 1] = (sspdpe[k - 1] - anpe * (d__1 * d__1)) / anpem1;
/* L190: */
	}
	i__2 = npe;
	for (ipe = 1; ipe <= i__2; ++ipe) {
	    alpwt = (float)0.;
	    for (k = 1; k <= 5; ++k) {
		if (sspdpe[k - 1] != (float)0.) {
/* Computing 2nd power */
		    d__1 = pdpe[k + ipe * 5 - 6] - ampdpe[k - 1];
		    alpwt += d__1 * d__1 / sspdpe[k - 1];
		}
/* L200: */
	    }
	    pwt[ipe - 1] = exp(-alpwt / (float)2.);
/* L210: */
	}
/* Calculates the reciprocal of the volatility weight. */
	i__2 = npe;
	for (ipe = 1; ipe <= i__2; ++ipe) {
	    idpi = idppe[ipe - 1];
	    zx = cfl1[(idpi << 1) + 1];
	    zy = cfl1[(idpi << 1) + 2];
/* Computing 2nd power */
	    d__1 = pdpe[ipe * 5 - 5] - zx;
/* Computing 2nd power */
	    d__2 = pdpe[ipe * 5 - 4] - zy;
/* Computing 2nd power */
	    d__3 = pdpe[ipe * 5 - 3];
/* Computing 2nd power */
	    d__4 = pdpe[ipe * 5 - 2];
/* Computing 2nd power */
	    d__5 = pdpe[ipe * 5 - 1];
	    rvwt[ipe - 1] = (d__1 * d__1 + d__2 * d__2) * (d__3 * d__3 + d__4 
		    * d__4 * (float)2. + d__5 * d__5);
/*             ZXX=0.0 */
/*             ZXY=0.0 */
/*             ZYY=0.0 */
/*             RVWT(IPE)=((PDPE(1,IPE)-ZX)**2+(PDPE(2,IPE)-ZY)**2)
 */
/*    +                 *((PDPE(3,IPE)-ZXX)**2+2.0*(PDPE(4,IPE)-ZX
Y)**2 */
/*    +                  +(PDPE(5,IPE)-ZYY)**2) */
/* L220: */
	}
/* Calculates the weighted values of partial derivatives. */
	for (k = 1; k <= 5; ++k) {
	    pddif[k - 1] = (float)0.;
	    pddii[k - 1] = (float)0.;
/* L230: */
	}
	smwtf = (float)0.;
	smwti = (float)0.;
	i__2 = npe;
	for (ipe = 1; ipe <= i__2; ++ipe) {
	    if (rvwt[ipe - 1] > (float)0.) {
		wtf = pwt[ipe - 1] / rvwt[ipe - 1];
		for (k = 1; k <= 5; ++k) {
		    pddif[k - 1] += pdpe[k + ipe * 5 - 6] * wtf;
/* L240: */
		}
		smwtf += wtf;
	    } else {
		wti = pwt[ipe - 1];
		for (k = 1; k <= 5; ++k) {
		    pddii[k - 1] += pdpe[k + ipe * 5 - 6] * wti;
/* L250: */
		}
		smwti += wti;
	    }
/* L260: */
	}
	if (smwti <= (float)0.) {
	    for (k = 1; k <= 5; ++k) {
		pdd[k + idp1 * 5] = pddif[k - 1] / smwtf;
/* L270: */
	    }
	} else {
	    for (k = 1; k <= 5; ++k) {
		pdd[k + idp1 * 5] = pddii[k - 1] / smwti;
/* L280: */
	    }
	}
	goto L310;
/* Only one qualified point set */
L290:
	for (k = 1; k <= 5; ++k) {
	    pdd[k + idp1 * 5] = pdpe[k - 1];
/* L300: */
	}
L310:
	;
    }
    return 0;
} /* sdpd3p_ */

/* Subroutine */ int sdcldp_(ndp, xd, yd, ipc, dsq, idsq)
integer *ndp;
doublereal *xd, *yd;
integer *ipc;
doublereal *dsq;
integer *idsq;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Local variables */
    static integer jipc, jdpmn;
    static doublereal dsqmn;
    static integer jipcmx, idsqmn, jdsqmn, idp, jdp;


/* Closest data points */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine selects, at each of the data points, nine data */
/* points closest to it. */

/* The input arguments are */
/*   NDP  = number of data points, */
/*   XD   = array of dimension NDP containing the x */
/*          coordinates of the data points, */
/*   YD   = array of dimension NDP containing the y */
/*          coordinates of the data points. */

/* The output argument is */
/*   IPC  = two-dimensional integer array of dimension 9*NDP, */
/*          where the point numbers of nine data points closest */
/*          to the IDPth data point, in an ascending order of */
/*          the distance from the IDPth point, are to be */
/*          stored in the IDPth column, where IDP = 1, 2, */
/*          ..., NDP. */

/* The other arguments are */
/*   DSQ  = array of dimension NDP used as a work area, */
/*   IDSQ = integer array of dimension NDP used as a work */
/*          area. */


/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* DO-loop with respect to the data point number */
    /* Parameter adjustments */
    --idsq;
    --dsq;
    ipc -= 10;
    --yd;
    --xd;

    /* Function Body */
    i__1 = *ndp;
    for (idp = 1; idp <= i__1; ++idp) {
/* Calculates the distance squared for all data points from the */
/* IDPth data point and stores the data point number and the */
/* calculated results in the IDSQ and DSQ arrays, respectively. */
	i__2 = *ndp;
	for (jdp = 1; jdp <= i__2; ++jdp) {
	    idsq[jdp] = jdp;
/* Computing 2nd power */
	    d__1 = xd[jdp] - xd[idp];
/* Computing 2nd power */
	    d__2 = yd[jdp] - yd[idp];
	    dsq[jdp] = d__1 * d__1 + d__2 * d__2;
/* L10: */
	}
/* Sorts the IDSQ and DSQ arrays in such a way that the IDPth */
/* point is in the first element in each array. */
	idsq[idp] = 1;
	dsq[idp] = dsq[1];
	idsq[1] = idp;
	dsq[1] = (float)0.;
/* Selects nine data points closest to the IDPth data point and */
/* stores the data point numbers in the IPC array. */
/* Computing MIN */
	i__2 = *ndp - 1;
	jipcmx = min(i__2,10);
	i__2 = jipcmx;
	for (jipc = 2; jipc <= i__2; ++jipc) {
	    jdsqmn = jipc;
	    dsqmn = dsq[jipc];
	    jdpmn = jipc + 1;
	    i__3 = *ndp;
	    for (jdp = jdpmn; jdp <= i__3; ++jdp) {
		if (dsq[jdp] < dsqmn) {
		    jdsqmn = jdp;
		    dsqmn = dsq[jdp];
		}
/* L20: */
	    }
	    idsqmn = idsq[jdsqmn];
	    idsq[jdsqmn] = idsq[jipc];
	    dsq[jdsqmn] = dsq[jipc];
	    idsq[jipc] = idsqmn;
/* L30: */
	}
	for (jipc = 1; jipc <= 9; ++jipc) {
	    ipc[jipc + idp * 9] = idsq[jipc + 1];
/* L40: */
	}
/* L50: */
    }
    return 0;
} /* sdcldp_ */

/* Subroutine */ int sdcf3p_(ndp, xd, yd, zd, ipc, cf, ncp)
integer *ndp;
doublereal *xd, *yd, *zd;
integer *ipc;
doublereal *cf;
integer *ncp;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer idpi;
    static doublereal b[10];
    static integer i, j, k[10];
    static doublereal x, y, x1, x2, y1, y2, z1, z2, ee[100]	/* was [10][
	    10] */, cn, aa1[9]	/* was [3][3] */, aa2[36]	/* was [6][6] 
	    */, aa3[100]	/* was [10][10] */, zz[100]	/* was [10][
	    10] */;
    extern /* Subroutine */ int sdleqn_();
    static doublereal cfi[10], det;
    static integer idp;


/* Coefficients of the third-degree polynomial for z(x,y) */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine calculates, for each data point, coefficients */
/* of the third-degree polynomial for z(x,y) fitted to the set of */
/* 10 data points consisting of the data point in question and */
/* nine data points closest to it.  When the condition number of */
/* the matrix associated with the 10 data point set is too large, */
/* this subroutine calculates coefficients of the second-degree */
/* polynomial fitted to the set of six data points consisting of */
/* the data point in question and five data points closest to it. */
/* When the condition number of the matrix associated with the six */
/* data point set is too large, this subroutine calculates */
/* coefficients of the first-degree polynomial fitted to the set of */
/* three data points closest to the data point in question.  When */
/* the condition number of the matrix associated with the three data */
/* point set is too large, this subroutine calculates coefficients */
/* of the first-degree polynomial fitted to the set of two data */
/* points consisting of the data point in question and one data */
/* point closest to it, assuming that the plane represented by the */
/* polynomial is horizontal in the direction which is at right */
/* angles to the line connecting the two data points. */

/* The input arguments are */
/*   NDP = number of data points, */
/*   XD  = array of dimension NDP containing the x */
/*         coordinates of the data points, */
/*   YD  = array of dimension NDP containing the y */
/*         coordinates of the data points, */
/*   ZD  = array of dimension NDP containing the z values */
/*         at the data points, */
/*   IPC = two-dimensional integer array of dimension */
/*         9*NDP containing the point numbers of 9 data */
/*         points closest to the IDPth data point in the */
/*         IDPth column, where IDP = 1, 2, ..., NDP. */

/* The output arguments are */
/*   CF  = two-dimensional array of dimension 9*NDP, */
/*         where the coefficients of the polynomial */
/*         (a10, a20, a30, a01, a11, a21, a02, a12, a03) */
/*         calculated at the IDPth data point are to be */
/*         stored in the IDPth column, where IDP = 1, 2, */
/*         ..., NDP, */
/*   NCP = integer array of dimension NDP, where the numbers */
/*         of the closest points used are to be stored. */

/* The constant in the first PARAMETER statement below is */
/*   CNRMX = maximum value of the ratio of the condition */
/*           number of the matrix associated with the point */
/*           set to the number of points. */
/* The constant value has been selected empirically. */

/* The N1, N2, and N3 constants in the second PARAMETER statement */
/* are the numbers of the data points used to determine the first-, */
/* second-, and third-degree polynomials, respectively. */

/* This subroutine calls the SDLEQN subroutine. */


/* Specification statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* Main DO-loop with respect to the data point */
    /* Parameter adjustments */
    --ncp;
    cf -= 10;
    ipc -= 10;
    --zd;
    --yd;
    --xd;

    /* Function Body */
    i__1 = *ndp;
    for (idp = 1; idp <= i__1; ++idp) {
	for (j = 1; j <= 9; ++j) {
	    cf[j + idp * 9] = (float)0.;
/* L10: */
	}
/* Calculates the coefficients of the set of linear equations */
/* with the 10-point data point set. */
	for (i = 1; i <= 10; ++i) {
	    if (i == 1) {
		idpi = idp;
	    } else {
		idpi = ipc[i - 1 + idp * 9];
	    }
	    x = xd[idpi];
	    y = yd[idpi];
	    aa3[i - 1] = (float)1.;
	    aa3[i + 9] = x;
	    aa3[i + 19] = x * x;
	    aa3[i + 29] = x * x * x;
	    aa3[i + 39] = y;
	    aa3[i + 49] = x * y;
	    aa3[i + 59] = x * x * y;
	    aa3[i + 69] = y * y;
	    aa3[i + 79] = x * y * y;
	    aa3[i + 89] = y * y * y;
	    b[i - 1] = zd[idpi];
/* L20: */
	}
/* Solves the set of linear equations. */
	sdleqn_(&c__10, aa3, b, cfi, &det, &cn, k, ee, zz);
/* Stores the calculated results as the coefficients of the */
/* third-degree polynomial when applicable. */
	if (det != (float)0.) {
	    if (cn <= 1.5e5) {
		for (j = 2; j <= 10; ++j) {
		    cf[j - 1 + idp * 9] = cfi[j - 1];
/* L30: */
		}
		ncp[idp] = 9;
		goto L60;
	    }
	}
/* Calculates the coefficients of the set of linear equations */
/* with the 6-point data point set. */
	for (i = 1; i <= 6; ++i) {
	    if (i == 1) {
		idpi = idp;
	    } else {
		idpi = ipc[i - 1 + idp * 9];
	    }
	    x = xd[idpi];
	    y = yd[idpi];
	    aa2[i - 1] = (float)1.;
	    aa2[i + 5] = x;
	    aa2[i + 11] = x * x;
	    aa2[i + 17] = y;
	    aa2[i + 23] = x * y;
	    aa2[i + 29] = y * y;
	    b[i - 1] = zd[idpi];
/* L40: */
	}
/* Solves the set of linear equations. */
	sdleqn_(&c__6, aa2, b, cfi, &det, &cn, k, ee, zz);
/* Stores the calculated results as the coefficients of the */
/* second-degree polynomial when applicable. */
	if (det != (float)0.) {
	    if (cn <= 9e4) {
		cf[idp * 9 + 1] = cfi[1];
		cf[idp * 9 + 2] = cfi[2];
		cf[idp * 9 + 4] = cfi[3];
		cf[idp * 9 + 5] = cfi[4];
		cf[idp * 9 + 7] = cfi[5];
		ncp[idp] = 5;
		goto L60;
	    }
	}
/* Calculates the coefficients of the set of linear equations */
/* with the 3-point data point set. */
	for (i = 1; i <= 3; ++i) {
	    idpi = ipc[i + idp * 9];
	    x = xd[idpi];
	    y = yd[idpi];
	    aa1[i - 1] = (float)1.;
	    aa1[i + 2] = x;
	    aa1[i + 5] = y;
	    b[i - 1] = zd[idpi];
/* L50: */
	}
/* Solves the set of linear equations. */
	sdleqn_(&c__3, aa1, b, cfi, &det, &cn, k, ee, zz);
/* Stores the calculated results as the coefficients of the */
/* first-degree polynomial when applicable. */
	if (det != (float)0.) {
	    if (cn <= 4.5e4) {
		cf[idp * 9 + 1] = cfi[1];
		cf[idp * 9 + 4] = cfi[2];
		ncp[idp] = 3;
		goto L60;
	    }
	}
/* Calculates the coefficients of the set of linear equations */
/* with the 2-point data point set when applicable. */
	idpi = idp;
	x1 = xd[idpi];
	y1 = yd[idpi];
	z1 = zd[idpi];
	idpi = ipc[idp * 9 + 1];
	x2 = xd[idpi];
	y2 = yd[idpi];
	z2 = zd[idpi];
/* Computing 2nd power */
	d__1 = x2 - x1;
/* Computing 2nd power */
	d__2 = y2 - y1;
	cf[idp * 9 + 1] = (x2 - x1) * (z2 - z1) / (d__1 * d__1 + d__2 * d__2);
/* Computing 2nd power */
	d__1 = x2 - x1;
/* Computing 2nd power */
	d__2 = y2 - y1;
	cf[idp * 9 + 4] = (y2 - y1) * (z2 - z1) / (d__1 * d__1 + d__2 * d__2);
	ncp[idp] = 1;
L60:
	;
    }
    return 0;
} /* sdcf3p_ */

/* Subroutine */ int sdleqn_(n, aa, b, x, det, cn, k, ee, zz)
integer *n;
doublereal *aa, *b, *x, *det, *cn;
integer *k;
doublereal *ee, *zz;
{
    /* System generated locals */
    integer aa_dim1, aa_offset, ee_dim1, ee_offset, zz_dim1, zz_offset, i__1, 
	    i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer pow_ii();
    double sqrt();

    /* Local variables */
    static doublereal aamx;
    static integer kjmx, i, j;
    static doublereal aaiij;
    static integer ij;
    static doublereal sa;
    static integer jj;
    static doublereal aaijij, aaijmx, sz;
    static integer ijr, jmx, ijp1;


/* Solution of a set of linear equations */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine solves a set of linear equations. */

/* The input arguments are */
/*   N   = number of linear equations, */
/*   AA  = two-dimensional array of dimension N*N */
/*         containing the coefficients of the equations, */
/*   B   = array of dimension N containing the constant */
/*         values in the right-hand side of the equations. */

/* The output arguments are */
/*   X   = array of dimension N, where the solution is */
/*         to be stored, */
/*   DET = determinant of the AA array, */
/*   CN  = condition number of the AA matrix. */

/* The other arguments are */
/*   K   = integer array of dimension N used internally */
/*         as the work area, */
/*   EE  = two-dimensional array of dimension N*N used */
/*         internally as the work area, */
/*   ZZ  = two-dimensional array of dimension N*N used */
/*         internally as the work area. */


/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* Calculation */
/* Initial setting */
    /* Parameter adjustments */
    zz_dim1 = *n;
    zz_offset = zz_dim1 + 1;
    zz -= zz_offset;
    ee_dim1 = *n;
    ee_offset = ee_dim1 + 1;
    ee -= ee_offset;
    --k;
    --x;
    --b;
    aa_dim1 = *n;
    aa_offset = aa_dim1 + 1;
    aa -= aa_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	k[j] = j;
/* L10: */
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    ee[i + j * ee_dim1] = (float)0.;
/* L20: */
	}
	ee[i + i * ee_dim1] = (float)1.;
/* L30: */
    }
/* Calculation of inverse matrix of AA */
    i__1 = *n;
    for (ij = 1; ij <= i__1; ++ij) {
/* Finds out the element having the maximum absolute value in the */
/* IJ th row. */
	aamx = (d__1 = aa[ij + ij * aa_dim1], abs(d__1));
	jmx = ij;
	i__2 = *n;
	for (j = ij; j <= i__2; ++j) {
	    if ((d__1 = aa[ij + j * aa_dim1], abs(d__1)) > aamx) {
		aamx = (d__1 = aa[ij + j * aa_dim1], abs(d__1));
		jmx = j;
	    }
/* L40: */
	}
/* Switches two columns in such a way that the element with the */
/* maximum value is on the diagonal. */
	i__2 = *n;
	for (i = 1; i <= i__2; ++i) {
	    aaijmx = aa[i + ij * aa_dim1];
	    aa[i + ij * aa_dim1] = aa[i + jmx * aa_dim1];
	    aa[i + jmx * aa_dim1] = aaijmx;
/* L50: */
	}
	kjmx = k[ij];
	k[ij] = k[jmx];
	k[jmx] = kjmx;
/* Makes the diagonal element to be unity. */
	aaijij = aa[ij + ij * aa_dim1];
	if (aaijij == (float)0.) {
	    goto L210;
	}
	i__2 = *n;
	for (j = ij; j <= i__2; ++j) {
	    aa[ij + j * aa_dim1] /= aaijij;
/* L60: */
	}
	i__2 = *n;
	for (jj = 1; jj <= i__2; ++jj) {
	    ee[ij + jj * ee_dim1] /= aaijij;
/* L70: */
	}
/* Eliminates the lower left elements. */
	if (ij < *n) {
	    ijp1 = ij + 1;
	    i__2 = *n;
	    for (i = ijp1; i <= i__2; ++i) {
		aaiij = aa[i + ij * aa_dim1];
		i__3 = *n;
		for (j = ijp1; j <= i__3; ++j) {
		    aa[i + j * aa_dim1] -= aa[ij + j * aa_dim1] * aaiij;
/* L80: */
		}
		i__3 = *n;
		for (jj = 1; jj <= i__3; ++jj) {
		    ee[i + jj * ee_dim1] -= ee[ij + jj * ee_dim1] * aaiij;
/* L90: */
		}
/* L100: */
	    }
	}
/* Calculates the determinant. */
	if (ij == 1) {
	    *det = (float)1.;
	}
	i__2 = ij + jmx;
	*det = *det * aaijij * pow_ii(&c_n1, &i__2);
/* L110: */
    }
/* Calculates the elements of the inverse matrix. */
    i__1 = *n;
    for (ijr = 1; ijr <= i__1; ++ijr) {
	ij = *n + 1 - ijr;
	if (ij < *n) {
	    ijp1 = ij + 1;
	    i__2 = *n;
	    for (j = ijp1; j <= i__2; ++j) {
		i__3 = *n;
		for (jj = 1; jj <= i__3; ++jj) {
		    ee[ij + jj * ee_dim1] -= aa[ij + j * aa_dim1] * ee[j + jj 
			    * ee_dim1];
/* L120: */
		}
/* L130: */
	    }
	}
/* L140: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i = k[j];
	i__2 = *n;
	for (jj = 1; jj <= i__2; ++jj) {
	    zz[i + jj * zz_dim1] = ee[j + jj * ee_dim1];
/* L150: */
	}
/* L160: */
    }
/* Calculation of the condition number of AA */
    sa = (float)0.;
    sz = (float)0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    sa += aa[i + j * aa_dim1] * aa[j + i * aa_dim1];
	    sz += zz[i + j * zz_dim1] * zz[j + i * zz_dim1];
/* L170: */
	}
/* L180: */
    }
    *cn = sqrt((d__1 = sa * sz, abs(d__1)));
/* Calculation of X vector */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	x[i] = (float)0.;
	i__2 = *n;
	for (j = 1; j <= i__2; ++j) {
	    x[i] += zz[i + j * zz_dim1] * b[j];
/* L190: */
	}
/* L200: */
    }
    return 0;
/* Special case where the determinant is zero */
L210:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	x[i] = (float)0.;
/* L220: */
    }
    *det = (float)0.;
    return 0;
} /* sdleqn_ */

/* Subroutine */ int sdls1p_(ndp, xd, yd, zd, ipc, ncp, cfl1)
integer *ndp;
doublereal *xd, *yd, *zd;
integer *ipc, *ncp;
doublereal *cfl1;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Local variables */
    static integer idpi, npls, i;
    static doublereal x, y, z, b1, b2, x1, x2, y1, y2, z1, z2, a11, a12, a22, 
	    an, sx, sy, sz;
    static integer idp;
    static doublereal dlt, sxx, sxy, sxz, syy, syz;


/* Least squares fit of a linear surface (plane) to z(x,y) values */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine performs the least squares fit of a linear */
/* surface (plane) to a data point set consisting of the data */
/* point in question and several data points closest to it used */
/* in the SDCF3P subroutine. */

/* The input arguments are */
/*   NDP  = number of data points, */
/*   XD   = array of dimension NDP containing the x coordinates */
/*          of the data points, */
/*   YD   = array of dimension NDP containing the y coordinates */
/*          of the data points, */
/*   ZD   = array of dimension NDP containing the z values at */
/*          the data points, */
/*   IPC  = two-dimensional integer array of dimension 9*NDP */
/*          containing, in the IDPth column, point numbers of */
/*          nine data points closest to the IDPth data point, */
/*          where IDP = 1, 2, ..., NDP, */
/*   NCP  = integer array of dimension NDP containing the */
/*          numbers of the closest points used in the SDCF3P */
/*          subroutine. */

/* The output argument is */
/*   CFL1 = two-dimensional array of dimension 2*NDP, where */
/*          the coefficients (a10, a01) of the least squares */
/*          fit, first-degree polynomial calculated at the */
/*          IDPth data point are to be stored in the IDPth */
/*          column, where IDP = 1, 2, ..., NDP. */

/* Before this subroutine is called, the SDCF3P subroutine must */
/* have been called. */


/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/* DO-loop with respect to the data point */
    /* Parameter adjustments */
    cfl1 -= 3;
    --ncp;
    ipc -= 10;
    --zd;
    --yd;
    --xd;

    /* Function Body */
    i__1 = *ndp;
    for (idp = 1; idp <= i__1; ++idp) {
	npls = ncp[idp] + 1;
	if (npls == 2) {
	    goto L20;
	}
/* Performs the least squares fit of a plane. */
	sx = (float)0.;
	sy = (float)0.;
	sxx = (float)0.;
	sxy = (float)0.;
	syy = (float)0.;
	sz = (float)0.;
	sxz = (float)0.;
	syz = (float)0.;
	i__2 = npls;
	for (i = 1; i <= i__2; ++i) {
	    if (i == 1) {
		idpi = idp;
	    } else {
		idpi = ipc[i - 1 + idp * 9];
	    }
	    x = xd[idpi];
	    y = yd[idpi];
	    z = zd[idpi];
	    sx += x;
	    sy += y;
	    sxx += x * x;
	    sxy += x * y;
	    syy += y * y;
	    sz += z;
	    sxz += x * z;
	    syz += y * z;
/* L10: */
	}
	an = (doublereal) npls;
	a11 = an * sxx - sx * sx;
	a12 = an * sxy - sx * sy;
	a22 = an * syy - sy * sy;
	b1 = an * sxz - sx * sz;
	b2 = an * syz - sy * sz;
	dlt = a11 * a22 - a12 * a12;
	cfl1[(idp << 1) + 1] = (b1 * a22 - b2 * a12) / dlt;
	cfl1[(idp << 1) + 2] = (b2 * a11 - b1 * a12) / dlt;
	goto L30;
L20:
	idpi = idp;
	x1 = xd[idpi];
	y1 = yd[idpi];
	z1 = zd[idpi];
	idpi = ipc[idp * 9 + 1];
	x2 = xd[idpi];
	y2 = yd[idpi];
	z2 = zd[idpi];
/* Computing 2nd power */
	d__1 = x2 - x1;
/* Computing 2nd power */
	d__2 = y2 - y1;
	cfl1[(idp << 1) + 1] = (x2 - x1) * (z2 - z1) / (d__1 * d__1 + d__2 * 
		d__2);
/* Computing 2nd power */
	d__1 = x2 - x1;
/* Computing 2nd power */
	d__2 = y2 - y1;
	cfl1[(idp << 1) + 2] = (y2 - y1) * (z2 - z1) / (d__1 * d__1 + d__2 * 
		d__2);
L30:
	;
    }
    return 0;
} /* sdls1p_ */

/* Subroutine */ int sdlctn_(ndp, xd, yd, nt, ipt, nl, ipl, nip, xi, yi, ktli,
	 itli)
integer *ndp;
doublereal *xd, *yd;
integer *nt, *ipt, *nl, *ipl, *nip;
doublereal *xi, *yi;
integer *ktli, *itli;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer ilii, itii;
    static doublereal x0, x1, x2, x3, y0, y1, y2, y3;
    static integer il1, il2, itlipv, ktlipv, ip1, ip2, ip3, iip;


/* Locating points in a scattered data point set */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine locates points in a scattered data point set in */
/* the x-y plane, i.e., determines to which triangle each of the */
/* points to be located belongs.  When a point to be located does */
/* not lie inside the data area, this subroutine determines the */
/* border line segment when the point lies in an outside rectangle, */
/* in an outside triangle, or in the overlap of two outside */
/* rectangles. */

/* The input arguments are */
/*   NDP  = number of data points, */
/*   XD   = array of dimension NDP containing the x */
/*          coordinates of the data points, */
/*   YD   = array of dimension NDP containing the y */
/*          coordinates of the data points, */
/*   NT   = number of triangles, */
/*   IPT  = two-dimensional integer array of dimension 3*NT */
/*          containing the point numbers of the vertexes of */
/*          the triangles, */
/*   NL   = number of border line segments, */
/*   IPL  = two-dimensional integer array of dimension 2*NL */
/*          containing the point numbers of the end points of */
/*          the border line segments, */
/*   NIP  = number of points to be located, */
/*   XI   = array of dimension NIP containing the x */
/*          coordinates of the points to be located, */
/*   YI   = array of dimension NIP containing the y */
/*          coordinates of the points to be located. */

/* The output arguments are */
/*   KTLI = integer array of dimension NIP, where the code */
/*          for the type of the piece of plane in which each */
/*          interpolated point lies is to be stored */
/*        = 1 for a triangle inside the data area */
/*        = 2 for a rectangle on the right-hand side of a */
/*            border line segment */
/*        = 3 for a triangle between two rectangles on the */
/*            right-hand side of two consecutive border line */
/*            segments */
/*        = 4 for a triangle which is an overlap of two */
/*            rectangles on the right-hand side of two */
/*            consecutive border line segments, */
/*   ITLI = integer array of dimension NIP, where the */
/*          triangle numbers or the (second) border line */
/*          segment numbers corresponding to the points to */
/*          be located are to be stored. */


/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Statement Functions .. */
/*     .. */
/* Statement Function definitions */
/*     .. */
/* Outermost DO-loop with respect to the points to be located */
    /* Parameter adjustments */
    --yd;
    --xd;
    ipt -= 4;
    ipl -= 3;
    --itli;
    --ktli;
    --yi;
    --xi;

    /* Function Body */
    i__1 = *nip;
    for (iip = 1; iip <= i__1; ++iip) {
	x0 = xi[iip];
	y0 = yi[iip];
	if (iip == 1) {
	    ktlipv = 0;
	    itlipv = 0;
	} else {
	    ktlipv = ktli[iip - 1];
	    itlipv = itli[iip - 1];
	}
/* Checks if in the same inside triangle as previous. */
	if (ktlipv == 1) {
	    itii = itlipv;
	    ip1 = ipt[itii * 3 + 1];
	    ip2 = ipt[itii * 3 + 2];
	    ip3 = ipt[itii * 3 + 3];
	    x1 = xd[ip1];
	    y1 = yd[ip1];
	    x2 = xd[ip2];
	    y2 = yd[ip2];
	    x3 = xd[ip3];
	    y3 = yd[ip3];
	    if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) >= (float)0. && 
		    (x2 - x0) * (y3 - y0) - (y2 - y0) * (x3 - x0) >= (float)
		    0. && (x3 - x0) * (y1 - y0) - (y3 - y0) * (x1 - x0) >= (
		    float)0.) {
		ktli[iip] = 1;
		itli[iip] = itii;
		goto L40;
	    }
	}
/* Locates inside the data area. */
	i__2 = *nt;
	for (itii = 1; itii <= i__2; ++itii) {
	    ip1 = ipt[itii * 3 + 1];
	    ip2 = ipt[itii * 3 + 2];
	    ip3 = ipt[itii * 3 + 3];
	    x1 = xd[ip1];
	    y1 = yd[ip1];
	    x2 = xd[ip2];
	    y2 = yd[ip2];
	    x3 = xd[ip3];
	    y3 = yd[ip3];
	    if ((x1 - x0) * (y2 - y0) - (y1 - y0) * (x2 - x0) >= (float)0. && 
		    (x2 - x0) * (y3 - y0) - (y2 - y0) * (x3 - x0) >= (float)
		    0. && (x3 - x0) * (y1 - y0) - (y3 - y0) * (x1 - x0) >= (
		    float)0.) {
		ktli[iip] = 1;
		itli[iip] = itii;
		goto L40;
	    }
/* L10: */
	}
/* Locates outside the data area. */
	i__2 = *nl;
	for (ilii = 1; ilii <= i__2; ++ilii) {
	    il1 = ilii;
	    il2 = il1 % *nl + 1;
	    ip1 = ipl[(il1 << 1) + 1];
	    ip2 = ipl[(il2 << 1) + 1];
	    ip3 = ipl[(il2 << 1) + 2];
	    x1 = xd[ip1];
	    y1 = yd[ip1];
	    x2 = xd[ip2];
	    y2 = yd[ip2];
	    x3 = xd[ip3];
	    y3 = yd[ip3];
	    if ((x1 - x0) * (y3 - y0) - (y1 - y0) * (x3 - x0) <= (float)0.) {
		if ((x1 - x2) * (y3 - y2) - (y1 - y2) * (x3 - x2) <= (float)
			0.) {
		    if ((x1 - x2) * (x0 - x2) + (y1 - y2) * (y0 - y2) <= (
			    float)0. && (x3 - x2) * (x0 - x2) + (y3 - y2) * (
			    y0 - y2) <= (float)0.) {
			ktli[iip] = 3;
			itli[iip] = il2;
			goto L40;
		    }
		}
		if ((x1 - x2) * (y3 - y2) - (y1 - y2) * (x3 - x2) >= (float)
			0.) {
		    if ((x1 - x2) * (x0 - x2) + (y1 - y2) * (y0 - y2) >= (
			    float)0. && (x3 - x2) * (x0 - x2) + (y3 - y2) * (
			    y0 - y2) >= (float)0.) {
			ktli[iip] = 4;
			itli[iip] = il2;
			goto L40;
		    }
		}
	    }
/* L20: */
	}
	i__2 = *nl;
	for (ilii = 1; ilii <= i__2; ++ilii) {
	    il2 = ilii;
	    ip2 = ipl[(il2 << 1) + 1];
	    ip3 = ipl[(il2 << 1) + 2];
	    x2 = xd[ip2];
	    y2 = yd[ip2];
	    x3 = xd[ip3];
	    y3 = yd[ip3];
	    if ((x2 - x0) * (y3 - y0) - (y2 - y0) * (x3 - x0) <= (float)0.) {
		if ((x3 - x2) * (x0 - x2) + (y3 - y2) * (y0 - y2) >= (float)
			0. && (x2 - x3) * (x0 - x3) + (y2 - y3) * (y0 - y3) >=
			 (float)0.) {
		    ktli[iip] = 2;
		    itli[iip] = il2;
		    goto L40;
		}
	    }
/* L30: */
	}
L40:
	;
    }
} /* sdlctn_ */

/* Subroutine */ int sdplnl_(ndp, xd, yd, zd, nt, ipt, nl, ipl, pdd, nip, xi, 
	yi, ktli, itli, zi, extrpi)
integer *ndp;
doublereal *xd, *yd, *zd;
integer *nt, *ipt, *nl, *ipl;
doublereal *pdd;
integer *nip;
doublereal *xi, *yi;
integer *ktli, *itli;
doublereal *zi;
logical *extrpi;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal lusq, lvsq, spuv, a, b, c, d;
    static integer i, k;
    static doublereal u, v, x[3], y[3], z[3];
    static integer itlii, ktlii;
    static doublereal e1, e2, g1, g2, h1, h2, h3, p0, p1, p2, p3, p4, p5, x0, 
	    y0, z0, aa, ab, bb, ad, bc, cc, cd, dd, p00, ap, bp, cp, dp, p01, 
	    p02, p03, p04, p05, p10, dx, dy, p11, p12, p13, p14, p20, p21, 
	    p22, p23, p30, p31, p32, p40, p41, p50;
    static integer ir;
    static doublereal pd[15]	/* was [5][3] */, zu[3], zv[3];
    static integer itlipv, ktlipv;
    static doublereal wt1, wt2;
    static integer idp, ili, iip;
    static doublereal dlt, xii, yii, zii, zuu[3], zuv[3], zvv[3], act2, bdt2, 
	    zii1, zii2, adbc;


/* Polynomials */
/* (a supporting subroutine of the SDBI3P/SDSF3P subroutine package) */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This subroutine determines a polynomial in x and y for each */
/* triangle or rectangle in the x-y plane and calculates the z */
/* value by evaluating the polynomial for the desired points, */
/* for bivariate interpolation and surface fitting for scattered */
/* data. */

/* The input arguments are */
/*   NDP  = number of data points, */
/*   XD   = array of dimension NDP containing the x */
/*          coordinates of the data points, */
/*   YD   = array of dimension NDP containing the y */
/*          coordinates of the data points, */
/*   ZD   = array of dimension NDP containing the z */
/*          values at the data points, */
/*   NT   = number of triangles, */
/*   IPT  = two-dimensional integer array of dimension 3*NT */
/*          containing the point numbers of the vertexes of */
/*          the triangles, */
/*   NL   = number of border line segments, */
/*   IPL  = two-dimensional integer array of dimension 2*NL */
/*          containing the point numbers of the end points of */
/*          the border line segments, */
/*   PDD  = two-dimensional array of dimension 5*NDP */
/*          containing the partial derivatives at the data */
/*          points, */
/*   NIP  = number of output points at which interpolation is */
/*          to be performed, */
/*   XI   = array of dimension NIP containing the x */
/*          coordinates of the output points, */
/*   YI   = array of dimension NIP containing the y */
/*          coordinates of the output points, */
/*   KTLI = integer array of dimension NIP, each element */
/*          containing the code for the type of the piece of */
/*          the plane in which each output point lies */
/*        = 1 for a triangle inside the data area */
/*        = 2 for a rectangle on the right-hand side of a */
/*            border line segment */
/*        = 3 for a triangle between two rectangles on the */
/*            right-hand side of two consecutive border */
/*            line segments */
/*        = 4 for the triangle which is an overlap of two */
/*            rectangles on the right-hand side of two */
/*            consecutive border line segments, */
/*   ITLI = integer array of dimension NIP containing the */
/*          triangle numbers or the (second) border line */
/*          segment numbers corresponding to the output */
/*          points. */

/* The output argument is */
/*   ZI   = array of dimension NIP, where the calculated z */
/*          values are to be stored. */
/*   EXTRPI = logical array of dimension NIP, indicating */
/*            if a point resides outside the convex hull (and its Z value 
*/
/*            has been extrapolated) */

/* Specification statements */
/*     .. Scalar Arguments .. */
/*     .. */
/*     .. Array Arguments .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/* Outermost DO-loop with respect to the output point */
    /* Parameter adjustments */
    pdd -= 6;
    --zd;
    --yd;
    --xd;
    ipt -= 4;
    ipl -= 3;
    --extrpi;
    --zi;
    --itli;
    --ktli;
    --yi;
    --xi;

    /* Function Body */
    i__1 = *nip;
    for (iip = 1; iip <= i__1; ++iip) {
	xii = xi[iip];
	yii = yi[iip];
	ktlii = ktli[iip];
	itlii = itli[iip];
	if (iip == 1) {
	    ktlipv = 0;
	    itlipv = 0;
	} else {
	    ktlipv = ktli[iip - 1];
	    itlipv = itli[iip - 1];
	}
/* Part 1.  Calculation of ZII by interpolation */
	if (ktlii == 1) {
/* Calculates the coefficients when necessary. */
	    if (ktlii != ktlipv || itlii != itlipv) {
/* Loads coordinate and partial derivative values at the */
/* vertexes. */
		for (i = 1; i <= 3; ++i) {
		    idp = ipt[i + itlii * 3];
		    x[i - 1] = xd[idp];
		    y[i - 1] = yd[idp];
		    z[i - 1] = zd[idp];
		    for (k = 1; k <= 5; ++k) {
			pd[k + i * 5 - 6] = pdd[k + idp * 5];
/* L10: */
		    }
/* L20: */
		}
/* Determines the coefficients for the coordinate system */
/* transformation from the x-y system to the u-v system */
/* and vice versa. */
		x0 = x[0];
		y0 = y[0];
		a = x[1] - x0;
		b = x[2] - x0;
		c = y[1] - y0;
		d = y[2] - y0;
		ad = a * d;
		bc = b * c;
		dlt = ad - bc;
		ap = d / dlt;
		bp = -b / dlt;
		cp = -c / dlt;
		dp = a / dlt;
/* Converts the partial derivatives at the vertexes of the */
/* triangle for the u-v coordinate system. */
		aa = a * a;
		act2 = a * (float)2. * c;
		cc = c * c;
		ab = a * b;
		adbc = ad + bc;
		cd = c * d;
		bb = b * b;
		bdt2 = b * (float)2. * d;
		dd = d * d;
		for (i = 1; i <= 3; ++i) {
		    zu[i - 1] = a * pd[i * 5 - 5] + c * pd[i * 5 - 4];
		    zv[i - 1] = b * pd[i * 5 - 5] + d * pd[i * 5 - 4];
		    zuu[i - 1] = aa * pd[i * 5 - 3] + act2 * pd[i * 5 - 2] + 
			    cc * pd[i * 5 - 1];
		    zuv[i - 1] = ab * pd[i * 5 - 3] + adbc * pd[i * 5 - 2] + 
			    cd * pd[i * 5 - 1];
		    zvv[i - 1] = bb * pd[i * 5 - 3] + bdt2 * pd[i * 5 - 2] + 
			    dd * pd[i * 5 - 1];
/* L30: */
		}
/* Calculates the coefficients of the polynomial. */
		p00 = z[0];
		p10 = zu[0];
		p01 = zv[0];
		p20 = zuu[0] * (float).5;
		p11 = zuv[0];
		p02 = zvv[0] * (float).5;
		h1 = z[1] - p00 - p10 - p20;
		h2 = zu[1] - p10 - zuu[0];
		h3 = zuu[1] - zuu[0];
		p30 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
		p40 = h1 * (float)-15. + h2 * (float)7. - h3;
		p50 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
		h1 = z[2] - p00 - p01 - p02;
		h2 = zv[2] - p01 - zvv[0];
		h3 = zvv[2] - zvv[0];
		p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
		p04 = h1 * (float)-15. + h2 * (float)7. - h3;
		p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
		lusq = aa + cc;
		lvsq = bb + dd;
		spuv = ab + cd;
		p41 = spuv * (float)5. / lusq * p50;
		p14 = spuv * (float)5. / lvsq * p05;
		h1 = zv[1] - p01 - p11 - p41;
		h2 = zuv[1] - p11 - p41 * (float)4.;
		p21 = h1 * (float)3. - h2;
		p31 = h1 * (float)-2. + h2;
		h1 = zu[2] - p10 - p11 - p14;
		h2 = zuv[2] - p11 - p14 * (float)4.;
		p12 = h1 * (float)3. - h2;
		p13 = h1 * (float)-2. + h2;
		e1 = (lvsq - spuv) / (lvsq - spuv + (lusq - spuv));
		e2 = (float)1. - e1;
		g1 = e1 * (float)5. - (float)2.;
		g2 = (float)1. - g1;
		h1 = (e1 * (p50 - p41) + e2 * (p05 - p14)) * (float)5. + (p41 
			+ p14);
		h2 = zvv[1] * (float).5 - p02 - p12;
		h3 = zuu[2] * (float).5 - p20 - p21;
		p22 = h1 + g1 * h2 + g2 * h3;
		p32 = h2 - p22;
		p23 = h3 - p22;
	    }
/* Converts XII and YII to u-v system. */
	    dx = xii - x0;
	    dy = yii - y0;
	    u = ap * dx + bp * dy;
	    v = cp * dx + dp * dy;
/* Evaluates the polynomial. */
	    p0 = p00 + v * (p01 + v * (p02 + v * (p03 + v * (p04 + v * p05))))
		    ;
	    p1 = p10 + v * (p11 + v * (p12 + v * (p13 + v * p14)));
	    p2 = p20 + v * (p21 + v * (p22 + v * p23));
	    p3 = p30 + v * (p31 + v * p32);
	    p4 = p40 + v * p41;
	    p5 = p50;
	    zi[iip] = p0 + u * (p1 + u * (p2 + u * (p3 + u * (p4 + u * p5))));
	    extrpi[iip] = FALSE_;
	}
/* Part 2.  Calculation of ZII by extrapolation in the rectangle */
	if (ktlii == 2) {
/* Calculates the coefficients when necessary. */
	    if (ktlii != ktlipv || itlii != itlipv) {
/* Loads coordinate and partial derivative values at the end 
*/
/* points of the border line segment. */
		for (i = 1; i <= 2; ++i) {
		    idp = ipl[i + (itlii << 1)];
		    x[i - 1] = xd[idp];
		    y[i - 1] = yd[idp];
		    z[i - 1] = zd[idp];
		    for (k = 1; k <= 5; ++k) {
			pd[k + i * 5 - 6] = pdd[k + idp * 5];
/* L40: */
		    }
/* L50: */
		}
/* Determines the coefficients for the coordinate system */
/* transformation from the x-y system to the u-v system */
/* and vice versa. */
		x0 = x[0];
		y0 = y[0];
		a = y[1] - y[0];
		b = x[1] - x[0];
		c = -b;
		d = a;
		ad = a * d;
		bc = b * c;
		dlt = ad - bc;
		ap = d / dlt;
		bp = -b / dlt;
		cp = -bp;
		dp = ap;
/* Converts the partial derivatives at the end points of the 
*/
/* border line segment for the u-v coordinate system. */
		aa = a * a;
		act2 = a * (float)2. * c;
		cc = c * c;
		ab = a * b;
		adbc = ad + bc;
		cd = c * d;
		bb = b * b;
		bdt2 = b * (float)2. * d;
		dd = d * d;
		for (i = 1; i <= 2; ++i) {
		    zu[i - 1] = a * pd[i * 5 - 5] + c * pd[i * 5 - 4];
		    zv[i - 1] = b * pd[i * 5 - 5] + d * pd[i * 5 - 4];
		    zuu[i - 1] = aa * pd[i * 5 - 3] + act2 * pd[i * 5 - 2] + 
			    cc * pd[i * 5 - 1];
		    zuv[i - 1] = ab * pd[i * 5 - 3] + adbc * pd[i * 5 - 2] + 
			    cd * pd[i * 5 - 1];
		    zvv[i - 1] = bb * pd[i * 5 - 3] + bdt2 * pd[i * 5 - 2] + 
			    dd * pd[i * 5 - 1];
/* L60: */
		}
/* Calculates the coefficients of the polynomial. */
		p00 = z[0];
		p10 = zu[0];
		p01 = zv[0];
		p20 = zuu[0] * (float).5;
		p11 = zuv[0];
		p02 = zvv[0] * (float).5;
		h1 = z[1] - p00 - p01 - p02;
		h2 = zv[1] - p01 - zvv[0];
		h3 = zvv[1] - zvv[0];
		p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
		p04 = h1 * (float)-15. + h2 * (float)7. - h3;
		p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
		h1 = zu[1] - p10 - p11;
		h2 = zuv[1] - p11;
		p12 = h1 * (float)3. - h2;
		p13 = h1 * (float)-2. + h2;
		p21 = (zuu[1] - zuu[0]) * (float).5;
	    }
/* Converts XII and YII to u-v system. */
	    dx = xii - x0;
	    dy = yii - y0;
	    u = ap * dx + bp * dy;
	    v = cp * dx + dp * dy;
/* Evaluates the polynomial. */
	    p0 = p00 + v * (p01 + v * (p02 + v * (p03 + v * (p04 + v * p05))))
		    ;
	    p1 = p10 + v * (p11 + v * (p12 + v * p13));
	    p2 = p20 + v * p21;
	    zi[iip] = p0 + u * (p1 + u * p2);
	    extrpi[iip] = TRUE_;
	}
/* Part 3.  Calculation of ZII by extrapolation in the triangle */
	if (ktlii == 3) {
/* Calculates the coefficients when necessary. */
	    if (ktlii != ktlipv || itlii != itlipv) {
/* Loads coordinate and partial derivative values at the verte
x */
/* of the triangle. */
		idp = ipl[(itlii << 1) + 1];
		x0 = xd[idp];
		y0 = yd[idp];
		z0 = zd[idp];
		for (k = 1; k <= 5; ++k) {
		    pd[k - 1] = pdd[k + idp * 5];
/* L70: */
		}
/* Calculates the coefficients of the polynomial. */
		p00 = z0;
		p10 = pd[0];
		p01 = pd[1];
		p20 = pd[2] * (float).5;
		p11 = pd[3];
		p02 = pd[4] * (float).5;
	    }
/* Converts XII and YII to U-V system. */
	    u = xii - x0;
	    v = yii - y0;
/* Evaluates the polynomial. */
	    p0 = p00 + v * (p01 + v * p02);
	    p1 = p10 + v * p11;
	    zi[iip] = p0 + u * (p1 + u * p20);
	    extrpi[iip] = TRUE_;
	}
/* Part 4.  Calculation of ZII by extrapolation in the triangle */
/*          which is an overlap of two rectangles. */
	if (ktlii == 4) {
/* Calculates the coefficients. */
	    for (ir = 1; ir <= 2; ++ir) {
		if (ir == 1) {
		    ili = (itlii + *nl - 2) % *nl + 1;
		} else {
		    ili = itlii;
		}
/* Loads coordinate and partial derivative values at the end 
*/
/* points of the border line segment. */
		for (i = 1; i <= 2; ++i) {
		    idp = ipl[i + (ili << 1)];
		    x[i - 1] = xd[idp];
		    y[i - 1] = yd[idp];
		    z[i - 1] = zd[idp];
		    for (k = 1; k <= 5; ++k) {
			pd[k + i * 5 - 6] = pdd[k + idp * 5];
/* L80: */
		    }
/* L90: */
		}
/* Determines the coefficients for the coordinate system */
/* transformation from the x-y system to the u-v system */
/* and vice versa. */
		x0 = x[0];
		y0 = y[0];
		a = y[1] - y[0];
		b = x[1] - x[0];
		c = -b;
		d = a;
		ad = a * d;
		bc = b * c;
		dlt = ad - bc;
		ap = d / dlt;
		bp = -b / dlt;
		cp = -bp;
		dp = ap;
/* Converts the partial derivatives at the end points of the 
*/
/* border line segment for the u-v coordinate system. */
		aa = a * a;
		act2 = a * (float)2. * c;
		cc = c * c;
		ab = a * b;
		adbc = ad + bc;
		cd = c * d;
		bb = b * b;
		bdt2 = b * (float)2. * d;
		dd = d * d;
		for (i = 1; i <= 2; ++i) {
		    zu[i - 1] = a * pd[i * 5 - 5] + c * pd[i * 5 - 4];
		    zv[i - 1] = b * pd[i * 5 - 5] + d * pd[i * 5 - 4];
		    zuu[i - 1] = aa * pd[i * 5 - 3] + act2 * pd[i * 5 - 2] + 
			    cc * pd[i * 5 - 1];
		    zuv[i - 1] = ab * pd[i * 5 - 3] + adbc * pd[i * 5 - 2] + 
			    cd * pd[i * 5 - 1];
		    zvv[i - 1] = bb * pd[i * 5 - 3] + bdt2 * pd[i * 5 - 2] + 
			    dd * pd[i * 5 - 1];
/* L100: */
		}
/* Calculates the coefficients of the polynomial. */
		p00 = z[0];
		p10 = zu[0];
		p01 = zv[0];
		p20 = zuu[0] * (float).5;
		p11 = zuv[0];
		p02 = zvv[0] * (float).5;
		h1 = z[1] - p00 - p01 - p02;
		h2 = zv[1] - p01 - zvv[0];
		h3 = zvv[1] - zvv[0];
		p03 = h1 * (float)10. - h2 * (float)4. + h3 * (float).5;
		p04 = h1 * (float)-15. + h2 * (float)7. - h3;
		p05 = h1 * (float)6. - h2 * (float)3. + h3 * (float).5;
		h1 = zu[1] - p10 - p11;
		h2 = zuv[1] - p11;
		p12 = h1 * (float)3. - h2;
		p13 = h1 * (float)-2. + h2;
		p21 = (zuu[1] - zuu[0]) * (float).5;
/* Converts XII and YII to u-v system. */
		dx = xii - x0;
		dy = yii - y0;
		u = ap * dx + bp * dy;
		v = cp * dx + dp * dy;
/* Evaluates the polynomial. */
		p0 = p00 + v * (p01 + v * (p02 + v * (p03 + v * (p04 + v * 
			p05))));
		p1 = p10 + v * (p11 + v * (p12 + v * p13));
		p2 = p20 + v * p21;
		zii = p0 + u * (p1 + u * p2);
		if (ir == 1) {
		    zii1 = zii;
/* Computing 2nd power */
		    d__1 = (x[0] - x[1]) * (xii - x[1]) + (y[0] - y[1]) * (
			    yii - y[1]);
		    wt2 = d__1 * d__1;
		} else {
		    zii2 = zii;
/* Computing 2nd power */
		    d__1 = (x[1] - x[0]) * (xii - x[0]) + (y[1] - y[0]) * (
			    yii - y[0]);
		    wt1 = d__1 * d__1;
		}
/* L110: */
	    }
	    zi[iip] = (wt1 * zii1 + wt2 * zii2) / (wt1 + wt2);
	    extrpi[iip] = TRUE_;
	}
/* L120: */
    }
} /* sdplnl_ */

