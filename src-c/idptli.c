/* ../src/idptli.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    integer itpv;
} idpi_;

#define idpi_1 idpi_

/* Subroutine */ int idptli_(xd, yd, zd, ndp, nt, ipt, nl, ipl, iti, xii, yii,
	 zii, missii)
doublereal *xd, *yd, *zd;
integer *ndp, *nt, *ipt, *nl, *ipl, *iti;
doublereal *xii, *yii, *zii;
logical *missii;
{
    /* System generated locals */
    static doublereal equiv_0[1];

    /* Local variables */
    static integer jipt;
    static doublereal a, b, c, d;
    static integer i;
    static doublereal u, v, x[3], y[3], z[3];
#define p5 (equiv_0)
    static doublereal x0, y0, ad, bc, ap, bp, cp, dp;
#define p50 (equiv_0)
    static doublereal dx, dy;
    static integer it0, idp, jpd;
    static doublereal dlt;
    static integer ntl;

/* THIS SUBROUTINE PERFORMS LINEAR PUNCTUAL INTERPOLATION, */
/* I.E., DETERMINES THE Z VALUE AT A POINT. */
/* THE INPUT PARAMETERS ARE */
/*     XD,YD,ZD = ARRAYS OF DIMENSION NDP CONTAINING THE X, */
/*           Y, AND Z COORDINATES OF THE DATA POINTS, WHERE */
/*           NDP IS THE NUMBER OF THE DATA POINTS, */
/*     NT  = NUMBER OF TRIANGLES, */
/*     IPT = INTEGER ARRAY OF DIMENSION 3*NT CONTAINING THE */
/*           POINT NUMBERS OF THE VERTEXES OF THE TRIANGLES, */
/*     NL  = NUMBER OF BORDER LINE SEGMENTS, */
/*     IPL = INTEGER ARRAY OF DIMENSION 3*NL CONTAINING THE */
/*           POINT NUMBERS OF THE END POINTS OF THE BORDER */
/*           LINE SEGMENTS AND THEIR RESPECTIVE TRIANGLE */
/*           NUMBERS, */
/*     ITI = TRIANGLE NUMBER OF THE TRIANGLE IN WHICH LIES */
/*           THE POINT FOR WHICH INTERPOLATION IS TO BE */
/*           PERFORMED, */
/*     XII,YII = X AND Y COORDINATES OF THE POINT FOR WHICH */
/*           INTERPOLATION IS TO BE PERFORMED. */
/* THE OUTPUT PARAMETERS ARE */
/*     ZII = INTERPOLATED Z VALUE. */
/*     MISSII = LOCIGAL INDICATING MISSING VALUE */
/* DECLARATION STATEMENTS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --zd;
    --yd;
    --xd;
    --ipt;
    --ipl;

    /* Function Body */
/* L10: */
    it0 = *iti;
    ntl = *nt + *nl;
    if (it0 <= ntl) {
	goto L20;
    }
    goto L40;
/* CALCULATION OF ZII BY INTERPOLATION. */
/* CHECKS IF THE NECESSARY COEFFICIENTS HAVE BEEN CALCULATED. */
L20:
    if (it0 == idpi_1.itpv) {
	goto L30;
    }
/* LOADS COORDINATE AND PARTIAL DERIVATIVE VALUES AT THE */
/* VERTEXES. */
/* L21: */
    jipt = (it0 - 1) * 3;
    jpd = 0;
    for (i = 1; i <= 3; ++i) {
	++jipt;
	idp = ipt[jipt];
	x[i - 1] = xd[idp];
	y[i - 1] = yd[idp];
	z[i - 1] = zd[idp];
/* L23: */
    }
/* DETERMINES THE COEFFICIENTS FOR THE COORDINATE SYSTEM */
/* TRANSFORMATION FROM THE X-Y SYSTEM TO THE U-V SYSTEM */
/* AND VICE VERSA. */
/* L24: */
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
/* CONVERTS XII AND YII TO U-V SYSTEM. */
L30:
    dx = *xii - x0;
    dy = *yii - y0;
    u = ap * dx + bp * dy;
    v = cp * dx + dp * dy;
/* EVALUATES THE INTERPOLATED PLANE */
/* ACCORDING TO */
/* |  U   V  ZII-Z1 | */
/* |  1   0   Z2-Z1 | = 0 */
/* |  0   1   Z3-Z1 | */

    *zii = z[0] + u * (z[1] - z[0]) + v * (z[2] - z[0]);
    *missii = FALSE_;
    return 0;
/* NO EXTRAPOLATION! */
L40:
    *zii = 0.;
    *missii = TRUE_;
    return 0;
} /* idptli_ */

#undef p50
#undef p5


