/* ../src/idcldp.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int idcldp_(ndp, xd, yd, ncp, ipc)
integer *ndp;
doublereal *xd, *yd;
integer *ncp, *ipc;
{
    /* Initialized data */

    static integer ncpmx = 25;
    static integer lun = 6;

    /* Format strings */
    static char fmt_2090[] = "(1x/\002 ***   IMPROPER INPUT PARAMETER VALUE(\
S).\002)";
    static char fmt_2091[] = "(1x/\002 ***   ALL COLLINEAR DATA POINTS.\002)";
    static char fmt_2092[] = "(\002   NDP =\002,i5,5x,\002NCP =\002,i5/\002 \
ERROR DETECTED IN ROUTINE   IDCLDP\002/)";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(), e_wsfe(), do_fio();

    /* Local variables */
    static doublereal dsqi;
    static integer ip2mn, ip3mn, nclpt;
    static doublereal dsqmn;
    static integer j1;
    static doublereal dsqmx;
    static integer j3, j4, j2;
    static doublereal x1, y1;
    static integer ip1, ip2, ip3;
    static doublereal dx12, dy12, dx13, dy13;
    static integer jmx, ipc0[25], ncp0, ndp0;
    static doublereal dsq0[25];

    /* Fortran I/O blocks */
    static cilist io___27 = { 0, 0, 0, fmt_2090, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_2091, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_2092, 0 };


/* THIS SUBROUTINE SELECTS SEVERAL DATA POINTS THAT ARE CLOSEST */
/* TO EACH OF THE DATA POINT. */
/* THE INPUT PARAMETERS ARE */
/*     NDP = NUMBER OF DATA POINTS, */
/*     XD,YD = ARRAYS OF DIMENSION NDP CONTAINING THE X AND Y */
/*           COORDINATES OF THE DATA POINTS, */
/*     NCP = NUMBER OF DATA POINTS CLOSEST TO EACH DATA */
/*           POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     IPC = INTEGER ARRAY OF DIMENSION NCP*NDP, WHERE THE */
/*           POINT NUMBERS OF NCP DATA POINTS CLOSEST TO */
/*           EACH OF THE NDP DATA POINTS ARE TO BE STORED. */
/* THIS SUBROUTINE ARBITRARILY SETS A RESTRICTION THAT NCP MUST */
/* NOT EXCEED 25. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* DECLARATION STATEMENTS */
    /* Parameter adjustments */
    --yd;
    --xd;
    --ipc;

    /* Function Body */
/* STATEMENT FUNCTION */
/* PRELIMINARY PROCESSING */
/* L10: */
    ndp0 = *ndp;
    ncp0 = *ncp;
    if (ndp0 < 2) {
	goto L90;
    }
    if (ncp0 < 1 || ncp0 > ncpmx || ncp0 >= ndp0) {
	goto L90;
    }
/* CALCULATION */
/* L20: */
    i__1 = ndp0;
    for (ip1 = 1; ip1 <= i__1; ++ip1) {
/* - SELECTS NCP POINTS. */
	x1 = xd[ip1];
	y1 = yd[ip1];
	j1 = 0;
	dsqmx = (float)0.;
	i__2 = ndp0;
	for (ip2 = 1; ip2 <= i__2; ++ip2) {
	    if (ip2 == ip1) {
		goto L22;
	    }
/* Computing 2nd power */
	    d__1 = xd[ip2] - x1;
/* Computing 2nd power */
	    d__2 = yd[ip2] - y1;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    ++j1;
	    dsq0[j1 - 1] = dsqi;
	    ipc0[j1 - 1] = ip2;
	    if (dsqi <= dsqmx) {
		goto L21;
	    }
	    dsqmx = dsqi;
	    jmx = j1;
L21:
	    if (j1 >= ncp0) {
		goto L23;
	    }
L22:
	    ;
	}
L23:
	ip2mn = ip2 + 1;
	if (ip2mn > ndp0) {
	    goto L30;
	}
	i__2 = ndp0;
	for (ip2 = ip2mn; ip2 <= i__2; ++ip2) {
	    if (ip2 == ip1) {
		goto L25;
	    }
/* Computing 2nd power */
	    d__1 = xd[ip2] - x1;
/* Computing 2nd power */
	    d__2 = yd[ip2] - y1;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    if (dsqi >= dsqmx) {
		goto L25;
	    }
	    dsq0[jmx - 1] = dsqi;
	    ipc0[jmx - 1] = ip2;
	    dsqmx = (float)0.;
	    i__3 = ncp0;
	    for (j1 = 1; j1 <= i__3; ++j1) {
		if (dsq0[j1 - 1] <= dsqmx) {
		    goto L24;
		}
		dsqmx = dsq0[j1 - 1];
		jmx = j1;
L24:
		;
	    }
L25:
	    ;
	}
/* - CHECKS IF ALL THE NCP+1 POINTS ARE COLLINEAR. */
L30:
	ip2 = ipc0[0];
	dx12 = xd[ip2] - x1;
	dy12 = yd[ip2] - y1;
	i__2 = ncp0;
	for (j3 = 2; j3 <= i__2; ++j3) {
	    ip3 = ipc0[j3 - 1];
	    dx13 = xd[ip3] - x1;
	    dy13 = yd[ip3] - y1;
	    if (dy13 * dx12 - dx13 * dy12 != (float)0.) {
		goto L50;
	    }
/* L31: */
	}
/* - SEARCHES FOR THE CLOSEST NONCOLLINEAR POINT. */
/* L40: */
	nclpt = 0;
	i__2 = ndp0;
	for (ip3 = 1; ip3 <= i__2; ++ip3) {
	    if (ip3 == ip1) {
		goto L43;
	    }
	    i__3 = ncp0;
	    for (j4 = 1; j4 <= i__3; ++j4) {
		if (ip3 == ipc0[j4 - 1]) {
		    goto L43;
		}
/* L41: */
	    }
	    dx13 = xd[ip3] - x1;
	    dy13 = yd[ip3] - y1;
	    if (dy13 * dx12 - dx13 * dy12 == (float)0.) {
		goto L43;
	    }
/* Computing 2nd power */
	    d__1 = xd[ip3] - x1;
/* Computing 2nd power */
	    d__2 = yd[ip3] - y1;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    if (nclpt == 0) {
		goto L42;
	    }
	    if (dsqi >= dsqmn) {
		goto L43;
	    }
L42:
	    nclpt = 1;
	    dsqmn = dsqi;
	    ip3mn = ip3;
L43:
	    ;
	}
	if (nclpt == 0) {
	    goto L91;
	}
	dsqmx = dsqmn;
	ipc0[jmx - 1] = ip3mn;
/* - REPLACES THE LOCAL ARRAY FOR THE OUTPUT ARRAY. */
L50:
	j1 = (ip1 - 1) * ncp0;
	i__2 = ncp0;
	for (j2 = 1; j2 <= i__2; ++j2) {
	    ++j1;
	    ipc[j1] = ipc0[j2 - 1];
/* L51: */
	}
/* L59: */
    }
    return 0;
/* ERROR EXIT */
L90:
    io___27.ciunit = lun;
    s_wsfe(&io___27);
    e_wsfe();
    goto L92;
L91:
    io___28.ciunit = lun;
    s_wsfe(&io___28);
    e_wsfe();
L92:
    io___29.ciunit = lun;
    s_wsfe(&io___29);
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ncp0, (ftnlen)sizeof(integer));
    e_wsfe();
    ipc[1] = 0;
    return 0;
/* FORMAT STATEMENTS FOR ERROR MESSAGES */
} /* idcldp_ */

