/* ../src/idbvip.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer nit;
} idlc_;

#define idlc_1 idlc_

struct {
    integer itpv;
} idpi_;

#define idpi_1 idpi_

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int idbvip_(md, ncp, ndp, xd, yd, zd, nip, xi, yi, zi, iwk, 
	wk, missi)
integer *md, *ncp, *ndp;
doublereal *xd, *yd, *zd;
integer *nip;
doublereal *xi, *yi, *zi;
integer *iwk;
doublereal *wk;
logical *missi;
{
    /* Initialized data */

    static integer lun = 6;

    /* Format strings */
    static char fmt_2090[] = "(1x/\002 ***   IMPROPER INPUT PARAMETER VALUE(\
S).\002/\002   MD =\002,i4,10x,\002NCP =\002,i6,10x,\002NDP =\002,i6,10x,\
\002NIP =\002,i6/\002 ERROR DETECTED IN ROUTINE   IDBVIP\002/)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer jwit, jwit0, i, jwipc, jwipl, ncppv, ndppv, jwiwk, nippv, 
	    jwipt, jwiwl, jwiwp, nl;
    extern /* Subroutine */ int idcldp_();
    static integer nt;
    extern /* Subroutine */ int idtang_();
    static logical linear;
    extern /* Subroutine */ int idlctn_(), idptli_(), idpdrv_(), idptip_();
    static integer md0, iip, ncp0, ndp0, nip0;

    /* Fortran I/O blocks */
    static cilist io___22 = { 0, 0, 0, fmt_2090, 0 };


/* THIS SUBROUTINE PERFORMS BIVARIATE INTERPOLATION WHEN THE PRO- */
/* JECTIONS OF THE DATA POINTS IN THE X-Y PLANE ARE IRREGULARLY */
/* DISTRIBUTED IN THE PLANE. */
/* THE INPUT PARAMETERS ARE */
/*     MD  = MODE OF COMPUTATION (MUST BE 1, 2, OR 3), */
/*         = 1 FOR NEW NCP AND/OR NEW XD-YD, */
/*         = 2 FOR OLD NCP, OLD XD-YD, NEW XI-YI, */
/*         = 3 FOR OLD NCP, OLD XD-YD, OLD XI-YI, */
/*     NCP = NUMBER OF ADDITIONAL DATA POINTS USED FOR ESTI- */
/*           MATING PARTIAL DERIVATIVES AT EACH DATA POINT */
/*           (MUST BE 2 OR GREATER, BUT SMALLER THAN NDP), */
/*     NDP = NUMBER OF DATA POINTS (MUST BE 4 OR GREATER), */
/*     XD  = ARRAY OF DIMENSION NDP CONTAINING THE X */
/*           COORDINATES OF THE DATA POINTS, */
/*     YD  = ARRAY OF DIMENSION NDP CONTAINING THE Y */
/*           COORDINATES OF THE DATA POINTS, */
/*     ZD  = ARRAY OF DIMENSION NDP CONTAINING THE Z */
/*           COORDINATES OF THE DATA POINTS, */
/*     NIP = NUMBER OF OUTPUT POINTS AT WHICH INTERPOLATION */
/*           IS TO BE PERFORMED (MUST BE 1 OR GREATER), */
/*     XI  = ARRAY OF DIMENSION NIP CONTAINING THE X */
/*           COORDINATES OF THE OUTPUT POINTS, */
/*     YI  = ARRAY OF DIMENSION NIP CONTAINING THE Y */
/*           COORDINATES OF THE OUTPUT POINTS. */
/* THE OUTPUT PARAMETER IS */
/*     ZI  = ARRAY OF DIMENSION NIP WHERE INTERPOLATED Z */
/*           VALUES ARE TO BE STORED. */
/*    MISSI = LOCICAL ARRAY, INDICATING IF EXTRAPOLATION OR MISSING VALUES
*/
/*            OUTSIDE CONVEX HULL WANTED */
/* THE OTHER PARAMETERS ARE */
/*     IWK = INTEGER ARRAY OF DIMENSION */
/*              MAX0(31,27+NCP)*NDP+NIP */
/*           USED INTERNALLY AS A WORK AREA, */
/*     WK  = ARRAY OF DIMENSION 8*NDP USED INTERNALLY AS A */
/*           WORK AREA. */
/* THE VERY FIRST CALL TO THIS SUBROUTINE AND THE CALL WITH A NEW */
/* NCP VALUE, A NEW NDP VALUE, AND/OR NEW CONTENTS OF THE XD AND */
/* YD ARRAYS MUST BE MADE WITH MD=1.  THE CALL WITH MD=2 MUST BE */
/* PRECEDED BY ANOTHER CALL WITH THE SAME NCP AND NDP VALUES AND */
/* WITH THE SAME CONTENTS OF THE XD AND YD ARRAYS.  THE CALL WITH */
/* MD=3 MUST BE PRECEDED BY ANOTHER CALL WITH THE SAME NCP, NDP, */
/* AND NIP VALUES AND WITH THE SAME CONTENTS OF THE XD, YD, XI, */
/* AND YI ARRAYS.  BETWEEN THE CALL WITH MD=2 OR MD=3 AND ITS */
/* PRECEDING CALL, THE IWK AND WK ARRAYS MUST NOT BE DISTURBED. */
/* USE OF A VALUE BETWEEN 3 AND 5 (INCLUSIVE) FOR NCP IS RECOM- */
/* MENDED UNLESS THERE ARE EVIDENCES THAT DICTATE OTHERWISE. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* THIS SUBROUTINE CALLS THE IDCLDP, IDLCTN, IDPDRV, IDPTIP, AND */
/* IDTANG SUBROUTINES. */
/* DECLARATION STATEMENTS */
    /* Parameter adjustments */
    --wk;
    --zd;
    --yd;
    --xd;
    --missi;
    --iwk;
    --zi;
    --yi;
    --xi;

    /* Function Body */
/* SETTING OF SOME INPUT PARAMETERS TO LOCAL VARIABLES. */
/* (FOR MD=1,2,3) */
/* L10: */
    md0 = *md;
    ncp0 = *ncp;
    ndp0 = *ndp;
    nip0 = *nip;
/* ERROR CHECK.  (FOR MD=1,2,3) */
/* L20: */
    if (md0 < 1 || md0 > 3) {
	goto L90;
    }
    if (ncp0 == 0) {
	linear = TRUE_;
	i__1 = *nip;
	for (i = 1; i <= i__1; ++i) {
	    missi[i] = TRUE_;
/* L21: */
	}
    }
    if (ncp0 == 1 || ncp0 >= ndp0) {
	goto L90;
    }
    if (ndp0 < 4) {
	goto L90;
    }
    if (nip0 < 1) {
	goto L90;
    }
    if (md0 >= 2) {
	goto L22;
    }
    iwk[1] = ncp0;
    iwk[2] = ndp0;
    goto L23;
L22:
    ncppv = iwk[1];
    ndppv = iwk[2];
    if (ncp0 != ncppv) {
	goto L90;
    }
    if (ndp0 != ndppv) {
	goto L90;
    }
L23:
    if (md0 >= 3) {
	goto L24;
    }
    iwk[3] = *nip;
    goto L30;
L24:
    nippv = iwk[3];
    if (nip0 != nippv) {
	goto L90;
    }
/* ALLOCATION OF STORAGE AREAS IN THE IWK ARRAY.  (FOR MD=1,2,3) */
L30:
    jwipt = 16;
    jwiwl = ndp0 * 6 + 1;
    jwiwk = jwiwl;
    jwipl = ndp0 * 24 + 1;
    jwiwp = ndp0 * 30 + 1;
    jwipc = ndp0 * 27 + 1;
/* Computing MAX */
    i__1 = 31, i__2 = ncp0 + 27;
    jwit0 = max(i__1,i__2) * ndp0;
/* TRIANGULATES THE X-Y PLANE.  (FOR MD=1) */
/* L40: */
    if (md0 > 1) {
	goto L50;
    }
    idtang_(&ndp0, &xd[1], &yd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &iwk[
	    jwiwl], &iwk[jwiwp], &wk[1]);
    iwk[5] = nt;
    iwk[6] = nl;
    if (nt == 0) {
	return 0;
    }
/* DETERMINES NCP POINTS CLOSEST TO EACH DATA POINT.  (FOR MD=1) */
L50:
    if (md0 > 1 || linear) {
	goto L60;
    }
    idcldp_(&ndp0, &xd[1], &yd[1], &ncp0, &iwk[jwipc]);
    if (iwk[jwipc] == 0) {
	return 0;
    }
/* LOCATES ALL POINTS AT WHICH INTERPOLATION IS TO BE PERFORMED. */
/* (FOR MD=1,2) */
L60:
    if (md0 == 3) {
	goto L70;
    }
    idlc_1.nit = 0;
    jwit = jwit0;
    i__1 = nip0;
    for (iip = 1; iip <= i__1; ++iip) {
	++jwit;
	idlctn_(&ndp0, &xd[1], &yd[1], &nt, &iwk[jwipt], &nl, &iwk[jwipl], &
		xi[iip], &yi[iip], &iwk[jwit], &iwk[jwiwk], &wk[1]);
/* L61: */
    }
/* ESTIMATES PARTIAL DERIVATIVES AT ALL DATA POINTS. */
/* (FOR MD=1,2,3) */
L70:
    if (! linear) {
	idpdrv_(&ndp0, &xd[1], &yd[1], &zd[1], &ncp0, &iwk[jwipc], &wk[1]);
    }
/* INTERPOLATES THE ZI VALUES.  (FOR MD=1,2,3) */
/* L80: */
    idpi_1.itpv = 0;
    jwit = jwit0;
    i__1 = nip0;
    for (iip = 1; iip <= i__1; ++iip) {
	++jwit;
	if (linear) {
	    idptli_(&xd[1], &yd[1], &zd[1], ndp, &nt, &iwk[jwipt], &nl, &iwk[
		    jwipl], &iwk[jwit], &xi[iip], &yi[iip], &zi[iip], &missi[
		    iip]);
	} else {
	    idptip_(&xd[1], &yd[1], &zd[1], ndp, &nt, &iwk[jwipt], &nl, &iwk[
		    jwipl], &wk[1], &iwk[jwit], &xi[iip], &yi[iip], &zi[iip], 
		    &missi[iip]);
	}
/* L81: */
    }
    return 0;
/* ERROR EXIT */
L90:
    io___22.ciunit = lun;
    s_wsfe(&io___22);
    do_fio(&c__1, (char *)&md0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ncp0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nip0, (ftnlen)sizeof(integer));
    e_wsfe();
    return 0;
/* FORMAT STATEMENT FOR ERROR MESSAGE */
} /* idbvip_ */

