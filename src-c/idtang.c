/* ../src/idtang.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* Subroutine */ int idtang_(ndp, xd, yd, nt, ipt, nl, ipl, iwl, iwp, wk)
integer *ndp;
doublereal *xd, *yd;
integer *nt, *ipt, *nl, *ipl, *iwl, *iwp;
doublereal *wk;
{
    /* Initialized data */

    static doublereal ratio = 1e-6;
    static integer nrep = 100;
    static integer lun = 6;

    /* Format strings */
    static char fmt_2090[] = "(1x/\002 ***   NDP LESS THAN 4.\002/\002   NDP\
 =\002,i5)";
    static char fmt_2091[] = "(1x/\002 ***   IDENTICAL DATA POINTS.\002/\002\
   NDP =\002,i5,5x,\002IP1 =\002,i5,5x,\002IP2 =\002,i5,5x,\002XD =\002,e12.\
4,5x,\002YD =\002,e12.4)";
    static char fmt_2092[] = "(1x/\002 ***   ALL COLLINEAR DATA POINTS.\002\
/\002   NDP =\002,i5)";
    static char fmt_2093[] = "(\002 ERROR DETECTED IN ROUTINE   IDTANG\002/)";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer nlfc, ip1p1;
    static doublereal dsq12, armn;
    static integer irep;
    static doublereal dsqi;
    static integer jp2t3, jp3t3, jpmn;
    static doublereal dxmn, dymn, xdmp, ydmp, armx;
    static integer ipti, it1t3, it2t3, jpmx;
    static doublereal dxmx, dymx;
    static integer ndpm1, ilft2, iplj1, iplj2, ipmn1, ipmn2, ipti1, ipti2, 
	    nlft2, nlnt3, nsht3, itt3r;
    static doublereal dsqmn;
    static integer ntt3p3;
    static doublereal dsqmx, x1, y1;
    static integer jwl1mn;
    static doublereal ar;
    static integer ip, jp;
    static doublereal dx, dy;
    static integer it;
    extern integer idxchg_();
    static integer ip1, ip2, jp1, jp2, ip3, nl0, nt0, ilf, jpc;
    static doublereal dx21, dy21;
    static integer nlf, itf[2], nln, nsh, ntf, jwl, its, ndp0, ipl1, ipl2, 
	    jlt3, ipt1, ipt2, ipt3, nlt3, jwl1, itt3, ntt3;

    /* Fortran I/O blocks */
    static cilist io___77 = { 0, 0, 0, fmt_2090, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_2091, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_2092, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_2093, 0 };


/* THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y */
/* PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA */
/* POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE */
/* BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS */
/* CORRESPONDING TO THE BORDER LINE SEGMENTS. */
/* AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE */
/* ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS */
/* OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE, */
/* LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE. */
/* THE LUN CONSTANT IN THE DATA INITIALIZATION STATEMENT IS THE */
/* LOGICAL UNIT NUMBER OF THE STANDARD OUTPUT UNIT AND IS, */
/* THEREFORE, SYSTEM DEPENDENT. */
/* THIS SUBROUTINE CALLS THE IDXCHG FUNCTION. */
/* THE INPUT PARAMETERS ARE */
/*     NDP = NUMBER OF DATA POINTS, */
/*     XD  = ARRAY OF DIMENSION NDP CONTAINING THE */
/*           X COORDINATES OF THE DATA POINTS, */
/*     YD  = ARRAY OF DIMENSION NDP CONTAINING THE */
/*           Y COORDINATES OF THE DATA POINTS. */
/* THE OUTPUT PARAMETERS ARE */
/*     NT  = NUMBER OF TRIANGLES, */
/*     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE */
/*           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH */
/*           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND, */
/*           (3*IT-1)ST, AND (3*IT)TH ELEMENTS, */
/*           IT=1,2,...,NT, */
/*     NL  = NUMBER OF BORDER LINE SEGMENTS, */
/*     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE */
/*           POINT NUMBERS OF THE END POINTS OF THE (IL)TH */
/*           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE */
/*           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND, */
/*           (3*IL-1)ST, AND (3*IL)TH ELEMENTS, */
/*           IL=1,2,..., NL. */
/* THE OTHER PARAMETERS ARE */
/*     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED */
/*           INTERNALLY AS A WORK AREA, */
/*     IWP = INTEGER ARRAY OF DIMENSION NDP USED */
/*           INTERNALLY AS A WORK AREA, */
/*     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A */
/*           WORK AREA. */
/* DECLARATION STATEMENTS */
    /* Parameter adjustments */
    --wk;
    --iwp;
    --iwl;
    --ipl;
    --ipt;
    --yd;
    --xd;

    /* Function Body */
/* STATEMENT FUNCTIONS */
/* PRELIMINARY PROCESSING */
/* L10: */
    ndp0 = *ndp;
    ndpm1 = ndp0 - 1;
    if (ndp0 < 4) {
	goto L90;
    }
/* DETERMINES THE CLOSEST PAIR OF DATA POINTS AND THEIR MIDPOINT. */
/* L20: */
/* Computing 2nd power */
    d__1 = xd[2] - xd[1];
/* Computing 2nd power */
    d__2 = yd[2] - yd[1];
    dsqmn = d__1 * d__1 + d__2 * d__2;
    ipmn1 = 1;
    ipmn2 = 2;
    i__1 = ndpm1;
    for (ip1 = 1; ip1 <= i__1; ++ip1) {
	x1 = xd[ip1];
	y1 = yd[ip1];
	ip1p1 = ip1 + 1;
	i__2 = ndp0;
	for (ip2 = ip1p1; ip2 <= i__2; ++ip2) {
/* Computing 2nd power */
	    d__1 = xd[ip2] - x1;
/* Computing 2nd power */
	    d__2 = yd[ip2] - y1;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    if (dsqi == (float)0.) {
		goto L91;
	    }
	    if (dsqi >= dsqmn) {
		goto L21;
	    }
	    dsqmn = dsqi;
	    ipmn1 = ip1;
	    ipmn2 = ip2;
L21:
	    ;
	}
/* L22: */
    }
    dsq12 = dsqmn;
    xdmp = (xd[ipmn1] + xd[ipmn2]) / (float)2.;
    ydmp = (yd[ipmn1] + yd[ipmn2]) / (float)2.;
/* SORTS THE OTHER (NDP-2) DATA POINTS IN ASCENDING ORDER OF */
/* DISTANCE FROM THE MIDPOINT AND STORES THE SORTED DATA POINT */
/* NUMBERS IN THE IWP ARRAY. */
/* L30: */
    jp1 = 2;
    i__1 = ndp0;
    for (ip1 = 1; ip1 <= i__1; ++ip1) {
	if (ip1 == ipmn1 || ip1 == ipmn2) {
	    goto L31;
	}
	++jp1;
	iwp[jp1] = ip1;
/* Computing 2nd power */
	d__1 = xd[ip1] - xdmp;
/* Computing 2nd power */
	d__2 = yd[ip1] - ydmp;
	wk[jp1] = d__1 * d__1 + d__2 * d__2;
L31:
	;
    }
    i__1 = ndpm1;
    for (jp1 = 3; jp1 <= i__1; ++jp1) {
	dsqmn = wk[jp1];
	jpmn = jp1;
	i__2 = ndp0;
	for (jp2 = jp1; jp2 <= i__2; ++jp2) {
	    if (wk[jp2] >= dsqmn) {
		goto L32;
	    }
	    dsqmn = wk[jp2];
	    jpmn = jp2;
L32:
	    ;
	}
	its = iwp[jp1];
	iwp[jp1] = iwp[jpmn];
	iwp[jpmn] = its;
	wk[jpmn] = wk[jp1];
/* L33: */
    }
/* IF NECESSARY, MODIFIES THE ORDERING IN SUCH A WAY THAT THE */
/* FIRST THREE DATA POINTS ARE NOT COLLINEAR. */
/* L35: */
    ar = dsq12 * ratio;
    x1 = xd[ipmn1];
    y1 = yd[ipmn1];
    dx21 = xd[ipmn2] - x1;
    dy21 = yd[ipmn2] - y1;
    i__1 = ndp0;
    for (jp = 3; jp <= i__1; ++jp) {
	ip = iwp[jp];
	if ((d__1 = (yd[ip] - y1) * dx21 - (xd[ip] - x1) * dy21, abs(d__1)) > 
		ar) {
	    goto L37;
	}
/* L36: */
    }
    goto L92;
L37:
    if (jp == 3) {
	goto L40;
    }
    jpmx = jp;
    jp = jpmx + 1;
    i__1 = jpmx;
    for (jpc = 4; jpc <= i__1; ++jpc) {
	--jp;
	iwp[jp] = iwp[jp - 1];
/* L38: */
    }
    iwp[3] = ip;
/* FORMS THE FIRST TRIANGLE.  STORES POINT NUMBERS OF THE VER- */
/* TEXES OF THE TRIANGLE IN THE IPT ARRAY, AND STORES POINT NUM- */
/* BERS OF THE BORDER LINE SEGMENTS AND THE TRIANGLE NUMBER IN */
/* THE IPL ARRAY. */
L40:
    ip1 = ipmn1;
    ip2 = ipmn2;
    ip3 = iwp[3];
    if ((yd[ip3] - yd[ip1]) * (xd[ip2] - xd[ip1]) - (xd[ip3] - xd[ip1]) * (yd[
	    ip2] - yd[ip1]) >= (float)0.) {
	goto L41;
    }
    ip1 = ipmn2;
    ip2 = ipmn1;
L41:
    nt0 = 1;
    ntt3 = 3;
    ipt[1] = ip1;
    ipt[2] = ip2;
    ipt[3] = ip3;
    nl0 = 3;
    nlt3 = 9;
    ipl[1] = ip1;
    ipl[2] = ip2;
    ipl[3] = 1;
    ipl[4] = ip2;
    ipl[5] = ip3;
    ipl[6] = 1;
    ipl[7] = ip3;
    ipl[8] = ip1;
    ipl[9] = 1;
/* ADDS THE REMAINING (NDP-3) DATA POINTS, ONE BY ONE. */
/* L50: */
    i__1 = ndp0;
    for (jp1 = 4; jp1 <= i__1; ++jp1) {
	ip1 = iwp[jp1];
	x1 = xd[ip1];
	y1 = yd[ip1];
/* - DETERMINES THE VISIBLE BORDER LINE SEGMENTS. */
	ip2 = ipl[1];
	jpmn = 1;
	dxmn = xd[ip2] - x1;
	dymn = yd[ip2] - y1;
/* Computing 2nd power */
	d__1 = dxmn;
/* Computing 2nd power */
	d__2 = dymn;
	dsqmn = d__1 * d__1 + d__2 * d__2;
	armn = dsqmn * ratio;
	jpmx = 1;
	dxmx = dxmn;
	dymx = dymn;
	dsqmx = dsqmn;
	armx = armn;
	i__2 = nl0;
	for (jp2 = 2; jp2 <= i__2; ++jp2) {
	    ip2 = ipl[jp2 * 3 - 2];
	    dx = xd[ip2] - x1;
	    dy = yd[ip2] - y1;
	    ar = dy * dxmn - dx * dymn;
	    if (ar > armn) {
		goto L51;
	    }
/* Computing 2nd power */
	    d__1 = dx;
/* Computing 2nd power */
	    d__2 = dy;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    if (ar >= -armn && dsqi >= dsqmn) {
		goto L51;
	    }
	    jpmn = jp2;
	    dxmn = dx;
	    dymn = dy;
	    dsqmn = dsqi;
	    armn = dsqmn * ratio;
L51:
	    ar = dy * dxmx - dx * dymx;
	    if (ar < -armx) {
		goto L52;
	    }
/* Computing 2nd power */
	    d__1 = dx;
/* Computing 2nd power */
	    d__2 = dy;
	    dsqi = d__1 * d__1 + d__2 * d__2;
	    if (ar <= armx && dsqi >= dsqmx) {
		goto L52;
	    }
	    jpmx = jp2;
	    dxmx = dx;
	    dymx = dy;
	    dsqmx = dsqi;
	    armx = dsqmx * ratio;
L52:
	    ;
	}
	if (jpmx < jpmn) {
	    jpmx += nl0;
	}
	nsh = jpmn - 1;
	if (nsh <= 0) {
	    goto L60;
	}
/* - SHIFTS (ROTATES) THE IPL ARRAY TO HAVE THE INVISIBLE BORDER */
/* - LINE SEGMENTS CONTAINED IN THE FIRST PART OF THE IPL ARRAY. */
	nsht3 = nsh * 3;
	i__2 = nsht3;
	for (jp2t3 = 3; jp2t3 <= i__2; jp2t3 += 3) {
	    jp3t3 = jp2t3 + nlt3;
	    ipl[jp3t3 - 2] = ipl[jp2t3 - 2];
	    ipl[jp3t3 - 1] = ipl[jp2t3 - 1];
	    ipl[jp3t3] = ipl[jp2t3];
/* L53: */
	}
	i__2 = nlt3;
	for (jp2t3 = 3; jp2t3 <= i__2; jp2t3 += 3) {
	    jp3t3 = jp2t3 + nsht3;
	    ipl[jp2t3 - 2] = ipl[jp3t3 - 2];
	    ipl[jp2t3 - 1] = ipl[jp3t3 - 1];
	    ipl[jp2t3] = ipl[jp3t3];
/* L54: */
	}
	jpmx -= nsh;
/* - ADDS TRIANGLES TO THE IPT ARRAY, UPDATES BORDER LINE */
/* - SEGMENTS IN THE IPL ARRAY, AND SETS FLAGS FOR THE BORDER */
/* - LINE SEGMENTS TO BE REEXAMINED IN THE IWL ARRAY. */
L60:
	jwl = 0;
	i__2 = nl0;
	for (jp2 = jpmx; jp2 <= i__2; ++jp2) {
	    jp2t3 = jp2 * 3;
	    ipl1 = ipl[jp2t3 - 2];
	    ipl2 = ipl[jp2t3 - 1];
	    it = ipl[jp2t3];
/* - - ADDS A TRIANGLE TO THE IPT ARRAY. */
	    ++nt0;
	    ntt3 += 3;
	    ipt[ntt3 - 2] = ipl2;
	    ipt[ntt3 - 1] = ipl1;
	    ipt[ntt3] = ip1;
/* - - UPDATES BORDER LINE SEGMENTS IN THE IPL ARRAY. */
	    if (jp2 != jpmx) {
		goto L61;
	    }
	    ipl[jp2t3 - 1] = ip1;
	    ipl[jp2t3] = nt0;
L61:
	    if (jp2 != nl0) {
		goto L62;
	    }
	    nln = jpmx + 1;
	    nlnt3 = nln * 3;
	    ipl[nlnt3 - 2] = ip1;
	    ipl[nlnt3 - 1] = ipl[1];
	    ipl[nlnt3] = nt0;
/* - - DETERMINES THE VERTEX THAT DOES NOT LIE ON THE BORDER */
/* - - LINE SEGMENTS. */
L62:
	    itt3 = it * 3;
	    ipti = ipt[itt3 - 2];
	    if (ipti != ipl1 && ipti != ipl2) {
		goto L63;
	    }
	    ipti = ipt[itt3 - 1];
	    if (ipti != ipl1 && ipti != ipl2) {
		goto L63;
	    }
	    ipti = ipt[itt3];
/* - - CHECKS IF THE EXCHANGE IS NECESSARY. */
L63:
	    if (idxchg_(&xd[1], &yd[1], ndp, &ip1, &ipti, &ipl1, &ipl2) == 0) 
		    {
		goto L64;
	    }
/* - - MODIFIES THE IPT ARRAY WHEN NECESSARY. */
	    ipt[itt3 - 2] = ipti;
	    ipt[itt3 - 1] = ipl1;
	    ipt[itt3] = ip1;
	    ipt[ntt3 - 1] = ipti;
	    if (jp2 == jpmx) {
		ipl[jp2t3] = it;
	    }
	    if (jp2 == nl0 && ipl[3] == it) {
		ipl[3] = nt0;
	    }
/* - - SETS FLAGS IN THE IWL ARRAY. */
	    jwl += 4;
	    iwl[jwl - 3] = ipl1;
	    iwl[jwl - 2] = ipti;
	    iwl[jwl - 1] = ipti;
	    iwl[jwl] = ipl2;
L64:
	    ;
	}
	nl0 = nln;
	nlt3 = nlnt3;
	nlf = jwl / 2;
	if (nlf == 0) {
	    goto L79;
	}
/* - IMPROVES TRIANGULATION. */
/* L70: */
	ntt3p3 = ntt3 + 3;
	i__2 = nrep;
	for (irep = 1; irep <= i__2; ++irep) {
	    i__3 = nlf;
	    for (ilf = 1; ilf <= i__3; ++ilf) {
		ilft2 = ilf << 1;
		ipl1 = iwl[ilft2 - 1];
		ipl2 = iwl[ilft2];
/* - - LOCATES IN THE IPT ARRAY TWO TRIANGLES ON BOTH SIDES OF
 */
/* - - THE FLAGGED LINE SEGMENT. */
		ntf = 0;
		i__4 = ntt3;
		for (itt3r = 3; itt3r <= i__4; itt3r += 3) {
		    itt3 = ntt3p3 - itt3r;
		    ipt1 = ipt[itt3 - 2];
		    ipt2 = ipt[itt3 - 1];
		    ipt3 = ipt[itt3];
		    if (ipl1 != ipt1 && ipl1 != ipt2 && ipl1 != ipt3) {
			goto L71;
		    }
		    if (ipl2 != ipt1 && ipl2 != ipt2 && ipl2 != ipt3) {
			goto L71;
		    }
		    ++ntf;
		    itf[ntf - 1] = itt3 / 3;
		    if (ntf == 2) {
			goto L72;
		    }
L71:
		    ;
		}
		if (ntf < 2) {
		    goto L76;
		}
/* - - DETERMINES THE VERTEXES OF THE TRIANGLES THAT DO NOT LI
E */
/* - - ON THE LINE SEGMENT. */
L72:
		it1t3 = itf[0] * 3;
		ipti1 = ipt[it1t3 - 2];
		if (ipti1 != ipl1 && ipti1 != ipl2) {
		    goto L73;
		}
		ipti1 = ipt[it1t3 - 1];
		if (ipti1 != ipl1 && ipti1 != ipl2) {
		    goto L73;
		}
		ipti1 = ipt[it1t3];
L73:
		it2t3 = itf[1] * 3;
		ipti2 = ipt[it2t3 - 2];
		if (ipti2 != ipl1 && ipti2 != ipl2) {
		    goto L74;
		}
		ipti2 = ipt[it2t3 - 1];
		if (ipti2 != ipl1 && ipti2 != ipl2) {
		    goto L74;
		}
		ipti2 = ipt[it2t3];
/* - - CHECKS IF THE EXCHANGE IS NECESSARY. */
L74:
		if (idxchg_(&xd[1], &yd[1], ndp, &ipti1, &ipti2, &ipl1, &ipl2)
			 == 0) {
		    goto L76;
		}
/* - - MODIFIES THE IPT ARRAY WHEN NECESSARY. */
		ipt[it1t3 - 2] = ipti1;
		ipt[it1t3 - 1] = ipti2;
		ipt[it1t3] = ipl1;
		ipt[it2t3 - 2] = ipti2;
		ipt[it2t3 - 1] = ipti1;
		ipt[it2t3] = ipl2;
/* - - SETS NEW FLAGS. */
		jwl += 8;
		iwl[jwl - 7] = ipl1;
		iwl[jwl - 6] = ipti1;
		iwl[jwl - 5] = ipti1;
		iwl[jwl - 4] = ipl2;
		iwl[jwl - 3] = ipl2;
		iwl[jwl - 2] = ipti2;
		iwl[jwl - 1] = ipti2;
		iwl[jwl] = ipl1;
		i__4 = nlt3;
		for (jlt3 = 3; jlt3 <= i__4; jlt3 += 3) {
		    iplj1 = ipl[jlt3 - 2];
		    iplj2 = ipl[jlt3 - 1];
		    if (iplj1 == ipl1 && iplj2 == ipti2 || iplj2 == ipl1 && 
			    iplj1 == ipti2) {
			ipl[jlt3] = itf[0];
		    }
		    if (iplj1 == ipl2 && iplj2 == ipti1 || iplj2 == ipl2 && 
			    iplj1 == ipti1) {
			ipl[jlt3] = itf[1];
		    }
/* L75: */
		}
L76:
		;
	    }
	    nlfc = nlf;
	    nlf = jwl / 2;
	    if (nlf == nlfc) {
		goto L79;
	    }
/* - - RESETS THE IWL ARRAY FOR THE NEXT ROUND. */
	    jwl = 0;
	    jwl1mn = nlfc + 1 << 1;
	    nlft2 = nlf << 1;
	    i__3 = nlft2;
	    for (jwl1 = jwl1mn; jwl1 <= i__3; jwl1 += 2) {
		jwl += 2;
		iwl[jwl - 1] = iwl[jwl1 - 1];
		iwl[jwl] = iwl[jwl1];
/* L77: */
	    }
	    nlf = jwl / 2;
/* L78: */
	}
L79:
	;
    }
/* REARRANGES THE IPT ARRAY SO THAT THE VERTEXES OF EACH TRIANGLE */
/* ARE LISTED COUNTER-CLOCKWISE. */
/* L80: */
    i__1 = ntt3;
    for (itt3 = 3; itt3 <= i__1; itt3 += 3) {
	ip1 = ipt[itt3 - 2];
	ip2 = ipt[itt3 - 1];
	ip3 = ipt[itt3];
	if ((yd[ip3] - yd[ip1]) * (xd[ip2] - xd[ip1]) - (xd[ip3] - xd[ip1]) * 
		(yd[ip2] - yd[ip1]) >= (float)0.) {
	    goto L81;
	}
	ipt[itt3 - 2] = ip2;
	ipt[itt3 - 1] = ip1;
L81:
	;
    }
    *nt = nt0;
    *nl = nl0;
    return 0;
/* ERROR EXIT */
L90:
    io___77.ciunit = lun;
    s_wsfe(&io___77);
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    e_wsfe();
    goto L93;
L91:
    io___78.ciunit = lun;
    s_wsfe(&io___78);
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ip1, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&ip2, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&x1, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&y1, (ftnlen)sizeof(doublereal));
    e_wsfe();
    goto L93;
L92:
    io___79.ciunit = lun;
    s_wsfe(&io___79);
    do_fio(&c__1, (char *)&ndp0, (ftnlen)sizeof(integer));
    e_wsfe();
L93:
    io___80.ciunit = lun;
    s_wsfe(&io___80);
    e_wsfe();
    *nt = 0;
    return 0;
/* FORMAT STATEMENTS */
} /* idtang_ */

