/* ../src/idxchg.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

integer idxchg_(x, y, ndp, i1, i2, i3, i4)
doublereal *x, *y;
integer *ndp, *i1, *i2, *i3, *i4;
{
    /* System generated locals */
    integer ret_val;
    doublereal d__1, d__2;
    static doublereal equiv_0[1], equiv_1[1], equiv_2[1], equiv_3[1], equiv_4[
	    1], equiv_5[1];

    /* Local variables */
    static doublereal u1, u2, u3, x1, y1, x2, y2, x3, y3, x4, y4, u4;
    static integer idx;
#define a1sq (equiv_2)
#define b1sq (equiv_3)
#define c1sq (equiv_0)
#define c2sq (equiv_0)
#define a3sq (equiv_1)
#define b2sq (equiv_1)
#define b3sq (equiv_2)
#define a4sq (equiv_3)
#define b4sq (equiv_4)
#define a2sq (equiv_4)
#define c4sq (equiv_5)
#define c3sq (equiv_5)
    static doublereal s1sq, s2sq, s3sq, s4sq;

/* THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO */
/* TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION */
/* BY C. L. LAWSON. */
/* THE INPUT PARAMETERS ARE */
/*     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA */
/*           POINTS, */
/*     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2, */
/*           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3 */
/*           AND P4 CONNECTED DIAGONALLY. */
/* THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX- */
/* CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE. */
/* DECLARATION STATEMENTS */
/* PRELIMINARY PROCESSING */
    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
/* L10: */
    x1 = x[*i1];
    y1 = y[*i1];
    x2 = x[*i2];
    y2 = y[*i2];
    x3 = x[*i3];
    y3 = y[*i3];
    x4 = x[*i4];
    y4 = y[*i4];
/* CALCULATION */
/* L20: */
    idx = 0;
    u3 = (y2 - y3) * (x1 - x3) - (x2 - x3) * (y1 - y3);
    u4 = (y1 - y4) * (x2 - x4) - (x1 - x4) * (y2 - y4);
    if (u3 * u4 <= (float)0.) {
	goto L30;
    }
    u1 = (y3 - y1) * (x4 - x1) - (x3 - x1) * (y4 - y1);
    u2 = (y4 - y2) * (x3 - x2) - (x4 - x2) * (y3 - y2);
/* Computing 2nd power */
    d__1 = x1 - x3;
/* Computing 2nd power */
    d__2 = y1 - y3;
    *a1sq = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
    d__1 = x4 - x1;
/* Computing 2nd power */
    d__2 = y4 - y1;
    *b1sq = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
    d__1 = x3 - x4;
/* Computing 2nd power */
    d__2 = y3 - y4;
    *c1sq = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
    d__1 = x2 - x4;
/* Computing 2nd power */
    d__2 = y2 - y4;
    *a2sq = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
    d__1 = x3 - x2;
/* Computing 2nd power */
    d__2 = y3 - y2;
    *b2sq = d__1 * d__1 + d__2 * d__2;
/* Computing 2nd power */
    d__1 = x2 - x1;
/* Computing 2nd power */
    d__2 = y2 - y1;
    *c3sq = d__1 * d__1 + d__2 * d__2;
    s1sq = u1 * u1 / (*c1sq * max(*a1sq,*b1sq));
    s2sq = u2 * u2 / (*c2sq * max(*a2sq,*b2sq));
    s3sq = u3 * u3 / (*c3sq * max(*a3sq,*b3sq));
    s4sq = u4 * u4 / (*c4sq * max(*a4sq,*b4sq));
    if (min(s1sq,s2sq) < min(s3sq,s4sq)) {
	idx = 1;
    }
L30:
    ret_val = idx;
    return ret_val;
} /* idxchg_ */

#undef c3sq
#undef c4sq
#undef a2sq
#undef b4sq
#undef a4sq
#undef b3sq
#undef b2sq
#undef a3sq
#undef c2sq
#undef c1sq
#undef b1sq
#undef a1sq


