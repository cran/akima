/* ../src/driver.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__30 = 30;
static integer c_b23 = 210000000;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__2 = 2;

/* Main program */ MAIN__()
{
    /* Initialized data */

    static doublereal zie[30]	/* was [6][5] */ = { 58.2,37.45,25.125,21.687,
	    17.171,12.,60.751,39.375,21.969,20.165,14.442,8.004,49.315,27.607,
	    16.364,12.983,8.227,5.267,71.203,41.386,22.338,12.236,7.519,2.965,
	    34.6,14.817,4.122,3.726,6.342,.6 };
    static char nmpr[6+1] = "TPSD3P";
    static char nmwf[6+1] = "WFSD3P";
    static doublereal xd[30] = { 11.16,0.,24.2,9.66,19.85,5.22,10.35,11.77,
	    19.72,15.1,0.,25.,20.87,25.,19.99,14.59,10.28,15.2,4.51,5.23,0.,
	    2.14,16.7,.51,6.08,25.,25.,21.67,14.9,3.31 };
    static doublereal yd[30] = { 1.24,0.,16.23,20.,10.72,14.66,4.11,10.47,
	    1.39,17.19,20.,3.87,20.,0.,4.62,8.71,15.16,0.,20.,10.72,4.48,
	    15.03,19.65,8.37,4.58,20.,11.87,14.36,3.12,.13 };
    static doublereal zd[30] = { 22.15,58.2,2.83,4.73,7.97,40.36,22.33,13.62,
	    16.83,12.57,34.6,8.74,5.74,12.,14.72,14.81,21.59,21.6,15.61,26.5,
	    61.77,53.1,6.31,49.43,35.74,.6,4.4,5.52,21.7,44.08 };
    static doublereal xi[6] = { 0.,5.,10.,15.,20.,25. };
    static doublereal yi[5] = { 0.,5.,10.,15.,20. };

    /* Format strings */
    static char fmt_9000[] = "(a6,/,/,/,/,\002   Input data        NDP =\002\
,i3,/,/,2(\002      I      XD     YD     ZD    \002))";
    static char fmt_9010[] = "(1x)";
    static char fmt_9020[] = "(2(5x,i2,2x,3f7.2,3x))";
    static char fmt_9030[] = "(a1,a6,14x,\002Part 1.  Program Check for SDBI\
3P\002)";
    static char fmt_9040[] = "(1x,/,/,/,/,\002Calculation by points\002,/,/,\
21x,\002ZI values\002,28x,\002Differences\002,/,/,21x,\002ZI1(XI,YI)\002,27x,\
\002DZI1(XI,YI)\002)";
    static char fmt_9050[] = "(\002   XI    YI=\002,34x,\002YI=\002,/,5x,2(2\
x,5f7.2),/)";
    static char fmt_9060[] = "(1x,/,f5.2,2x,5f7.2,2x,5f7.2)";
    static char fmt_9070[] = "(1x,/,/,/,/,\002Calculation by columns\002,/,/\
,21x,\002ZI values\002,28x,\002Differences\002,/,/,21x,\002ZI2(XI,YI)\002,27\
x,\002DZI2(XI,YI)\002)";
    static char fmt_9080[] = "(a1,a6,14x,\002Part 2.  Program Check for SDSF\
3P\002)";
    static char fmt_9090[] = "(1x,/,/,/,/,\002Calculation with MD=1\002,/,/,\
21x,\002ZI values\002,28x,\002Differences\002,/,/,21x,\002ZI3(XI,YI)\002,27x,\
\002DZI3(XI,YI)\002)";
    static char fmt_9100[] = "(1x,/,/,/,/,\002Calculation with MD=2\002,/,/,\
21x,\002ZI values\002,28x,\002Differences\002,/,/,21x,\002ZI4(XI,YI)\002,27x,\
\002DZI4(XI,YI)\002)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;
    olist o__1;
    static doublereal equiv_0[795];

    /* Builtin functions */
    integer f_open(), s_wsfe(), do_fio(), e_wsfe();
    /* Subroutine */ int s_stop();

    /* Local variables */
    static integer ndpo2, l;
    extern /* Subroutine */ int sdbi3p_(), sdsf3p_();
    static integer md;
#define wk (equiv_0 + 285)
    static doublereal zi1[30]	/* was [6][5] */, zi2[30]	/* was [6][5] 
	    */, zi3[30]	/* was [6][5] */, zi4[30]	/* was [6][5] */;
    static integer idp, ier, ixi, iyi;
#define iwk ((integer *)equiv_0)
    static doublereal yip[6];
    static logical extrapi[30]	/* was [6][5] */;
    static doublereal dzi1[30]	/* was [6][5] */, dzi2[30]	/* was [6][5] 
	    */, dzi3[30]	/* was [6][5] */, dzi4[30]	/* was [6][5] 
	    */;

    /* Fortran I/O blocks */
    static cilist io___11 = { 0, 6, 0, fmt_9000, 0 };
    static cilist io___14 = { 0, 6, 0, fmt_9010, 0 };
    static cilist io___15 = { 0, 6, 0, fmt_9020, 0 };
    static cilist io___24 = { 0, 6, 0, fmt_9030, 0 };
    static cilist io___25 = { 0, 6, 0, fmt_9040, 0 };
    static cilist io___26 = { 0, 6, 0, fmt_9050, 0 };
    static cilist io___27 = { 0, 6, 0, fmt_9060, 0 };
    static cilist io___31 = { 0, 6, 0, fmt_9070, 0 };
    static cilist io___32 = { 0, 6, 0, fmt_9050, 0 };
    static cilist io___33 = { 0, 6, 0, fmt_9060, 0 };
    static cilist io___36 = { 0, 6, 0, fmt_9080, 0 };
    static cilist io___37 = { 0, 6, 0, fmt_9090, 0 };
    static cilist io___38 = { 0, 6, 0, fmt_9050, 0 };
    static cilist io___39 = { 0, 6, 0, fmt_9060, 0 };
    static cilist io___42 = { 0, 6, 0, fmt_9100, 0 };
    static cilist io___43 = { 0, 6, 0, fmt_9050, 0 };
    static cilist io___44 = { 0, 6, 0, fmt_9060, 0 };



/* Test Program for the SDBI3P/SDSF3P subroutine package */

/* Hiroshi Akima */
/* U.S. Department of Commerce, NTIA/ITS */
/* Version of 1995/05 */

/* This program calls the SDBI3P and SDSF3P subroutines. */

/* This program requires no input data files. */

/* This program creates the WFSD3P file.  All elements of the */
/* DZI1, DZI2, DZI3, and DZI4 arrays in this file are expected */
/* to be zero. */

/* Specification Statements */
/*     .. Parameters .. */
/*     .. */
/*     .. Local Scalars .. */
/*     .. */
/*     .. Local Arrays .. */
/*     .. */
/*     .. External Subroutines .. */
/*     .. */
/*     .. Intrinsic Functions .. */
/*     .. */
/*     .. Equivalences .. */
/*     .. */
/* Data statements */
/*     .. */
/* Opens the output file. */
    o__1.oerr = 0;
    o__1.ounit = 6;
    o__1.ofnmlen = 6;
    o__1.ofnm = nmwf;
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/* Writes the input data. */
    s_wsfe(&io___11);
    do_fio(&c__1, nmpr, 6L);
    do_fio(&c__1, (char *)&c__30, (ftnlen)sizeof(integer));
    e_wsfe();
    ndpo2 = 15;
    i__1 = ndpo2;
    for (l = 1; l <= i__1; ++l) {
	if (l % 5 == 1) {
	    s_wsfe(&io___14);
	    e_wsfe();
	}
	s_wsfe(&io___15);
	i__2 = ndpo2;
	for (idp = l; i__2 < 0 ? idp >= 30 : idp <= 30; idp += i__2) {
	    do_fio(&c__1, (char *)&idp, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&xd[idp - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&yd[idp - 1], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zd[idp - 1], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
/* L10: */
    }
/* Calculates and writes the output results. */
/* Part 1.  Program check for SDBI3P */
/*   Subpart 1.1.  Calculation of each ZI value at a point */
    for (iyi = 1; iyi <= 5; ++iyi) {
	for (ixi = 1; ixi <= 6; ++ixi) {
	    if (ixi == 1 && iyi == 1) {
		md = 1;
	    } else {
		md = 3;
	    }
	    sdbi3p_(&md, &c__30, xd, yd, zd, &c__1, &xi[ixi - 1], &yi[iyi - 1]
		    , &zi1[ixi + iyi * 6 - 7], &ier, wk, iwk, extrapi);
	    if (ier > 0) {
		s_stop("", 0L);
	    }
	    dzi1[ixi + iyi * 6 - 7] = (d__1 = zi1[ixi + iyi * 6 - 7] - zie[
		    ixi + iyi * 6 - 7], abs(d__1));
/* L20: */
	}
/* L30: */
    }
    s_wsfe(&io___24);
    do_fio(&c__1, (char *)&c_b23, (ftnlen)sizeof(integer));
    do_fio(&c__1, nmpr, 6L);
    e_wsfe();
    s_wsfe(&io___25);
    e_wsfe();
    s_wsfe(&io___26);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___27);
    for (ixi = 1; ixi <= 6; ++ixi) {
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&zi1[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&dzi1[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
    }
    e_wsfe();
/*   Subpart 1.2.  Calculation of ZI values for each YI value */
    for (iyi = 1; iyi <= 5; ++iyi) {
	if (iyi == 1) {
	    md = 1;
	} else {
	    md = 3;
	}
	for (ixi = 1; ixi <= 6; ++ixi) {
	    yip[ixi - 1] = yi[iyi - 1];
/* L40: */
	}
	sdbi3p_(&md, &c__30, xd, yd, zd, &c__6, xi, yip, &zi2[iyi * 6 - 6], &
		ier, wk, iwk, extrapi);
	if (ier > 0) {
	    s_stop("", 0L);
	}
	for (ixi = 1; ixi <= 6; ++ixi) {
	    dzi2[ixi + iyi * 6 - 7] = (d__1 = zi2[ixi + iyi * 6 - 7] - zie[
		    ixi + iyi * 6 - 7], abs(d__1));
/* L50: */
	}
/* L60: */
    }
    s_wsfe(&io___31);
    e_wsfe();
    s_wsfe(&io___32);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___33);
    for (ixi = 1; ixi <= 6; ++ixi) {
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&zi2[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&dzi2[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
    }
    e_wsfe();
/* Part 2.  Program check for SDSF3P */
/*   Subpart 2.1.  Calculation with MD=1 */
    sdsf3p_(&c__1, &c__30, xd, yd, zd, &c__6, xi, &c__5, yi, zi3, &ier, wk, 
	    iwk, extrapi);
    if (ier > 0) {
	s_stop("", 0L);
    }
    for (iyi = 1; iyi <= 5; ++iyi) {
	for (ixi = 1; ixi <= 6; ++ixi) {
	    dzi3[ixi + iyi * 6 - 7] = (d__1 = zi3[ixi + iyi * 6 - 7] - zie[
		    ixi + iyi * 6 - 7], abs(d__1));
/* L70: */
	}
/* L80: */
    }
    s_wsfe(&io___36);
    do_fio(&c__1, (char *)&c_b23, (ftnlen)sizeof(integer));
    do_fio(&c__1, nmpr, 6L);
    e_wsfe();
    s_wsfe(&io___37);
    e_wsfe();
    s_wsfe(&io___38);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___39);
    for (ixi = 1; ixi <= 6; ++ixi) {
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&zi3[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&dzi3[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
    }
    e_wsfe();
/*   Subpart 2.2.  Calculation with MD=2 */
    sdsf3p_(&c__2, &c__30, xd, yd, zd, &c__6, xi, &c__5, yi, zi4, &ier, wk, 
	    iwk, extrapi);
    if (ier > 0) {
	s_stop("", 0L);
    }
    for (iyi = 1; iyi <= 5; ++iyi) {
	for (ixi = 1; ixi <= 6; ++ixi) {
	    dzi4[ixi + iyi * 6 - 7] = (d__1 = zi4[ixi + iyi * 6 - 7] - zie[
		    ixi + iyi * 6 - 7], abs(d__1));
/* L90: */
	}
/* L100: */
    }
    s_wsfe(&io___42);
    e_wsfe();
    s_wsfe(&io___43);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___44);
    for (ixi = 1; ixi <= 6; ++ixi) {
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&zi4[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	for (iyi = 1; iyi <= 5; ++iyi) {
	    do_fio(&c__1, (char *)&dzi4[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
    }
    e_wsfe();
    s_stop("", 0L);
/* Format Statements */
} /* MAIN__ */

#undef iwk
#undef wk


/* Main program alias */ int tpsd3p_ () { MAIN__ (); }
