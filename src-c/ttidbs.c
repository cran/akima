/* ../src/ttidbs.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__5 = 5;

/* Main program */ MAIN__()
{
    /* Initialized data */

    static integer ncp = 4;
    static doublereal zi[30]	/* was [6][5] */ = { 58.2,39.55,26.9,21.71,
	    17.68,12.,61.58,39.39,22.04,21.29,14.36,8.04,59.18,27.39,16.78,
	    13.25,8.59,5.36,52.82,40.27,22.76,16.61,7.4,2.88,34.6,14.05,4.12,
	    3.17,6.31,.6 };
    static integer lun = 6;
    static integer ndp = 30;
    static doublereal xd[30] = { 11.16,24.2,19.85,10.35,19.72,0.,20.87,19.99,
	    10.28,4.51,0.,16.7,6.08,25.,14.9,0.,9.66,5.22,11.77,15.1,25.,25.,
	    14.59,15.2,5.23,2.14,.51,25.,21.67,3.31 };
    static doublereal yd[30] = { 1.24,16.23,10.72,4.11,1.39,20.,20.,4.62,
	    15.16,20.,4.48,19.65,4.58,11.87,3.12,0.,20.,14.66,10.47,17.19,
	    3.87,0.,8.71,0.,10.72,15.03,8.37,20.,14.36,.13 };
    static doublereal zd[30] = { 22.15,2.83,7.97,22.33,16.83,34.6,5.74,14.72,
	    21.59,15.61,61.77,6.31,35.74,4.4,21.7,58.2,4.73,40.36,13.62,12.57,
	    8.74,12.,14.81,21.6,26.5,53.1,49.43,.6,5.52,44.08 };
    static integer nxi = 6;
    static integer nyi = 5;
    static doublereal xi[6] = { 0.,5.,10.,15.,20.,25. };
    static doublereal yi[5] = { 0.,5.,10.,15.,20. };

    /* Format strings */
    static char fmt_2020[] = "(\0021\002,\002TTIDBS\002/////3x,\002INPUT D\
ATA\002,8x,\002NDP =\002,i3///\002      I      XD     YD     ZD \002/)";
    static char fmt_2021[] = "(1x)";
    static char fmt_2022[] = "(5x,i2,2x,3f7.2)";
    static char fmt_2030[] = "(\0021\002,\002TTIDBS\002/////3x,\002IDBVIP SU\
BROUTINE\002///26x,\002ZI1(XI,YI)\002)";
    static char fmt_2031[] = "(7x,\002XI\002,4x,\002YI=\002/12x,5f7.2/)";
    static char fmt_2032[] = "(1x/1x,f9.2,2x,5f7.2)";
    static char fmt_2040[] = "(1x/////3x,\002DIFFERENCE\002///25x,\002DZI1(X\
I,YI)\002)";
    static char fmt_2050[] = "(\0021\002,\002TTIDBS\002/////3x,\002IDSFFT SU\
BROUTINE\002///26x,\002ZI2(XI,YI)\002)";
    static char fmt_2060[] = "(1x/////3x,\002DIFFERENCE\002///25x,\002DZI2(X\
I,YI)\002)";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();
    /* Subroutine */ int s_stop();

    /* Local variables */
    static logical missi[30]	/* was [6][5] */;
    static integer md;
    static doublereal wk[240];
    extern /* Subroutine */ int idbvip_(), idsfft_();
    static doublereal zi1[30]	/* was [6][5] */, zi2[30]	/* was [6][5] 
	    */;
    static integer idp, ixi, iwk[1030], iyi;
    static doublereal dzi1[30]	/* was [6][5] */, dzi2[30]	/* was [6][5] 
	    */;

    /* Fortran I/O blocks */
    static cilist io___22 = { 0, 0, 0, fmt_2020, 0 };
    static cilist io___24 = { 0, 0, 0, fmt_2021, 0 };
    static cilist io___25 = { 0, 0, 0, fmt_2022, 0 };
    static cilist io___26 = { 0, 0, 0, fmt_2030, 0 };
    static cilist io___27 = { 0, 0, 0, fmt_2031, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_2032, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_2040, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_2031, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_2032, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_2050, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_2031, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_2032, 0 };
    static cilist io___35 = { 0, 0, 0, fmt_2060, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_2031, 0 };
    static cilist io___37 = { 0, 0, 0, fmt_2032, 0 };


/*    PROGRAM  TTIDBS(OUTPUT,TAPE6=OUTPUT)                              ID
000070*/
/*THIS PROGRAM IS A TEST PROGRAM FOR THE IDBVIP/IDSFFT SUBPRO-          ID
000080*/
/*GRAM PACKAGE.  ALL ELEMENTS OF RESULTING DZI1 AND DZI2 ARRAYS         ID
000090*/
/*ARE EXPECTED TO BE ZERO.                                              ID
000100*/
/*THE LUN CONSTANT IN THE LAST DATA INITIALIZATION STATEMENT IS         ID
000110*/
/*THE LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS,           ID
000120*/
/*THEREFORE, SYSTEM DEPENDENT.                                          ID
000130*/
/*DECLARATION STATEMENTS                                                ID
000140*/
/*CALCULATION                                                           ID
000670*/
/* L10: */
    md = 1;
    i__1 = nyi;
    for (iyi = 1; iyi <= i__1; ++iyi) {
	i__2 = nxi;
	for (ixi = 1; ixi <= i__2; ++ixi) {
	    if (ixi != 1 || iyi != 1) {
		md = 2;
	    }
	    missi[ixi + iyi * 6 - 7] = FALSE_;
	    idbvip_(&md, &ncp, &ndp, xd, yd, zd, &c__1, &xi[ixi - 1], &yi[iyi 
		    - 1], &zi1[ixi + iyi * 6 - 7], iwk, wk, missi);
/* L11: */
	}
/* L12: */
    }
/* L15: */
    idsfft_(&c__1, &ncp, &ndp, xd, yd, zd, &nxi, &nyi, xi, yi, zi2, iwk, wk, 
	    missi);
    i__1 = nyi;
    for (iyi = 1; iyi <= i__1; ++iyi) {
	i__2 = nxi;
	for (ixi = 1; ixi <= i__2; ++ixi) {
	    dzi1[ixi + iyi * 6 - 7] = (d__1 = zi1[ixi + iyi * 6 - 7] - zi[ixi 
		    + iyi * 6 - 7], abs(d__1));
	    dzi2[ixi + iyi * 6 - 7] = (d__1 = zi2[ixi + iyi * 6 - 7] - zi[ixi 
		    + iyi * 6 - 7], abs(d__1));
/* L16: */
	}
/* L17: */
    }
/*PRINTING OF INPUT DATA                                                ID
000830*/
/* L20: */
    io___22.ciunit = lun;
    s_wsfe(&io___22);
    do_fio(&c__1, (char *)&ndp, (ftnlen)sizeof(integer));
    e_wsfe();
    i__1 = ndp;
    for (idp = 1; idp <= i__1; ++idp) {
	if (idp % 5 == 1) {
	    io___24.ciunit = lun;
	    s_wsfe(&io___24);
	    e_wsfe();
	}
	io___25.ciunit = lun;
	s_wsfe(&io___25);
	do_fio(&c__1, (char *)&idp, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&xd[idp - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&yd[idp - 1], (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zd[idp - 1], (ftnlen)sizeof(doublereal));
	e_wsfe();
/* L23: */
    }
/*PRINTING OF OUTPUT RESULTS                                            ID
000890*/
/* L30: */
    io___26.ciunit = lun;
    s_wsfe(&io___26);
    e_wsfe();
    io___27.ciunit = lun;
    s_wsfe(&io___27);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nxi;
    for (ixi = 1; ixi <= i__1; ++ixi) {
	io___28.ciunit = lun;
	s_wsfe(&io___28);
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	i__2 = nyi;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    do_fio(&c__1, (char *)&zi1[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsfe();
/* L33: */
    }
/* L40: */
    io___29.ciunit = lun;
    s_wsfe(&io___29);
    e_wsfe();
    io___30.ciunit = lun;
    s_wsfe(&io___30);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nxi;
    for (ixi = 1; ixi <= i__1; ++ixi) {
	io___31.ciunit = lun;
	s_wsfe(&io___31);
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	i__2 = nyi;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    do_fio(&c__1, (char *)&dzi1[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsfe();
/* L43: */
    }
/* L50: */
    io___32.ciunit = lun;
    s_wsfe(&io___32);
    e_wsfe();
    io___33.ciunit = lun;
    s_wsfe(&io___33);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nxi;
    for (ixi = 1; ixi <= i__1; ++ixi) {
	io___34.ciunit = lun;
	s_wsfe(&io___34);
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	i__2 = nyi;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    do_fio(&c__1, (char *)&zi2[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsfe();
/* L53: */
    }
/* L60: */
    io___35.ciunit = lun;
    s_wsfe(&io___35);
    e_wsfe();
    io___36.ciunit = lun;
    s_wsfe(&io___36);
    do_fio(&c__5, (char *)&yi[0], (ftnlen)sizeof(doublereal));
    e_wsfe();
    i__1 = nxi;
    for (ixi = 1; ixi <= i__1; ++ixi) {
	io___37.ciunit = lun;
	s_wsfe(&io___37);
	do_fio(&c__1, (char *)&xi[ixi - 1], (ftnlen)sizeof(doublereal));
	i__2 = nyi;
	for (iyi = 1; iyi <= i__2; ++iyi) {
	    do_fio(&c__1, (char *)&dzi2[ixi + iyi * 6 - 7], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsfe();
/* L63: */
    }
    s_stop("", 0L);
/*FORMAT STATEMENTS                                                     ID
001110*/
} /* MAIN__ */

/* Main program alias */ int ttidbs_ () { MAIN__ (); }
