/* ../src/tripack.subset.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal y;
} stcom_;

#define stcom_1 stcom_

struct {
    doublereal swtol;
} swpcom_;

#define swpcom_1 swpcom_

/* Table of constant values */

static integer c__1 = 1;

/*      ALGORITHM 751, COLLECTED ALGORITHMS FROM ACM. */
/*      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE, */
/*      VOL. 22, NO. 1, March, 1996, P.  1--8. */

/*      This is not the complete tripack package! */
/* Subroutine */ int addnod_(k, xk, yk, ist, ncc, lcc, n, x, y, list, lptr, 
	lend, lnew, ier)
integer *k;
doublereal *xk, *yk;
integer *ist, *ncc, *lcc, *n;
doublereal *x, *y;
integer *list, *lptr, *lend, *lnew, *ier;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int swap_();
    static integer i, l;
    extern logical crtri_();
    static integer i1, i2, i3, lccip1, kk;
    extern /* Subroutine */ int bdyadd_();
    static integer lp;
    extern /* Subroutine */ int intadd_();
    extern integer indxcc_();
    extern /* Subroutine */ int trfind_();
    static integer in1, io1, io2, nm1;
    extern integer lstptr_();
    extern logical swptst_();
    static integer ibk, lpf, lpo1;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   08/25/91 */

/*   Given a triangulation of N nodes in the plane created by */
/* subroutine TRMESH or TRMSHR, this subroutine updates the */
/* data structure with the addition of a new node in position */
/* K.  If node K is inserted into X and Y (K .LE. N) rather */
/* than appended (K = N+1), then a corresponding insertion */
/* must be performed in any additional arrays associated */
/* with the nodes.  For example, an array of data values Z */
/* must be shifted down to open up position K for the new */
/* value:  set Z(I+1) to Z(I) for I = N,N-1,...,K.  For */
/* optimal efficiency, new nodes should be appended whenever */
/* possible.  Insertion is necessary, however, to add a non- */
/* constraint node when constraints are present (refer to */
/* subroutine ADDCST). */

/*   Note that a constraint node cannot be added by this */
/* routine.  In order to insert a constraint node, it is */
/* necessary to add the node with no constraints present */
/* (call this routine with NCC = 0), update LCC by increment- */
/* ing the appropriate entries, and then create (or restore) */
/* the constraints by a call to ADDCST. */

/*   The algorithm consists of the following steps:  node K */
/* is located relative to the triangulation (TRFIND), its */
/* index is added to the data structure (INTADD or BDYADD), */
/* and a sequence of swaps (SWPTST and SWAP) are applied to */
/* the arcs opposite K so that all arcs incident on node K */
/* and opposite node K (excluding constraint arcs) are local- */
/* ly optimal (satisfy the circumcircle test).  Thus, if a */
/* (constrained) Delaunay triangulation is input, a (con- */
/* strained) Delaunay triangulation will result.  All indexes */
/* are incremented as necessary for an insertion. */


/* On input: */

/*       K = Nodal index (index for X, Y, and LEND) of the */
/*           new node to be added.  1 .LE. K .LE. LCC(1). */
/*           (K .LE. N+1 if NCC=0). */

/*       XK,YK = Cartesian coordinates of the new node (to be */
/*               stored in X(K) and Y(K)).  The node must not */
/*               lie in a constraint region. */

/*       IST = Index of a node at which TRFIND begins the */
/*             search.  Search time depends on the proximity */
/*             of this node to node K.  1 .LE. IST .LE. N. */

/*       NCC = Number of constraint curves.  NCC .GE. 0. */

/* The above parameters are not altered by this routine. */

/*       LCC = List of constraint curve starting indexes (or */
/*             dummy array of length 1 if NCC = 0).  Refer to */
/*             subroutine ADDCST. */

/*       N = Number of nodes in the triangulation before K is */
/*           added.  N .GE. 3.  Note that N will be incre- */
/*           mented following the addition of node K. */

/*       X,Y = Arrays of length at least N+1 containing the */
/*             Cartesian coordinates of the nodes in the */
/*             first N positions with non-constraint nodes */
/*             in the first LCC(1)-1 locations if NCC > 0. */

/*       LIST,LPTR,LEND,LNEW = Data structure associated with */
/*                             the triangulation of nodes 1 */
/*                             to N.  The arrays must have */
/*                             sufficient length for N+1 */
/*                             nodes.  Refer to TRMESH. */

/* On output: */

/*       LCC = List of constraint curve starting indexes in- */
/*             cremented by 1 to reflect the insertion of K */
/*             unless NCC = 0 or IER .NE. 0. */

/*       N = Number of nodes in the triangulation including K */
/*           unless IER .NE. 0.  Note that all comments refer */
/*           to the input value of N. */

/*       X,Y = Arrays updated with the insertion of XK and YK */
/*             in the K-th positions (node I+1 was node I be- */
/*             fore the insertion for I = K to N if K .LE. N) */
/*             unless IER .NE. 0. */

/*       LIST,LPTR,LEND,LNEW = Data structure updated with */
/*                             the addition of node K unless */
/*                             IER .NE. 0. */

/*       IER = Error indicator: */
/*             IER =  0 if no errors were encountered. */
/*             IER = -1 if K, IST, NCC, N, or an LCC entry is */
/*                      outside its valid range on input. */
/*             IER = -2 if all nodes (including K) are col- */
/*                      linear. */
/*             IER =  L if nodes L and K coincide for some L. */
/*             IER = -3 if K lies in a constraint region. */

/*             The errors conditions are tested in the order */
/*             specified. */

/* Modules required by ADDNOD:  BDYADD, CRTRI, INDXCC, */
/*                                INSERT, INTADD, LEFT, */
/*                                LSTPTR, SWAP, SWPTST, */
/*                                TRFIND */

/* Intrinsic function called by ADDNOD:  ABS */

/* *********************************************************** */

    /* Parameter adjustments */
    --lend;
    --lptr;
    --list;
    --y;
    --x;
    --lcc;

    /* Function Body */
    kk = *k;

/* Test for an invalid input parameter. */

    if (kk < 1 || *ist < 1 || *ist > *n || *ncc < 0 || *n < 3) {
	goto L7;
    }
    lccip1 = *n + 1;
    for (i = *ncc; i >= 1; --i) {
	if (lccip1 - lcc[i] < 3) {
	    goto L7;
	}
	lccip1 = lcc[i];
/* L1: */
    }
    if (kk > lccip1) {
	goto L7;
    }

/* Find a triangle (I1,I2,I3) containing K or the rightmost */
/*   (I1) and leftmost (I2) visible boundary nodes as viewed */
/*   from node K. */

    trfind_(ist, xk, yk, &x[1], &y[1], &list[1], &lptr[1], &lend[1], &i1, &i2,
	     &i3);

/* Test for collinear nodes, duplicate nodes, and K lying in */
/*   a constraint region. */

    if (i1 == 0) {
	goto L8;
    }
    if (i3 != 0) {
	l = i1;
	if (*xk == x[l] && *yk == y[l]) {
	    goto L9;
	}
	l = i2;
	if (*xk == x[l] && *yk == y[l]) {
	    goto L9;
	}
	l = i3;
	if (*xk == x[l] && *yk == y[l]) {
	    goto L9;
	}
	if (*ncc > 0 && crtri_(ncc, &lcc[1], &i1, &i2, &i3)) {
	    goto L10;
	}
    } else {

/*   K is outside the convex hull of the nodes and lies in a */
/*     constraint region iff an exterior constraint curve is */
/*     present. */

	if (*ncc > 0 && indxcc_(ncc, &lcc[1], n, &list[1], &lend[1]) != 0) {
	    goto L10;
	}
    }

/* No errors encountered. */

    *ier = 0;
    nm1 = *n;
    ++(*n);
    if (kk < *n) {

/* Open a slot for K in X, Y, and LEND, and increment all */
/*   nodal indexes which are greater than or equal to K. */
/*   Note that LIST, LPTR, and LNEW are not yet updated with */
/*   either the neighbors of K or the edges terminating on K. */

	i__1 = kk;
	for (ibk = nm1; ibk >= i__1; --ibk) {
	    x[ibk + 1] = x[ibk];
	    y[ibk + 1] = y[ibk];
	    lend[ibk + 1] = lend[ibk];
/* L2: */
	}
	i__1 = *ncc;
	for (i = 1; i <= i__1; ++i) {
	    ++lcc[i];
/* L3: */
	}
	l = *lnew - 1;
	i__1 = l;
	for (i = 1; i <= i__1; ++i) {
	    if (list[i] >= kk) {
		++list[i];
	    }
	    if (list[i] <= -kk) {
		--list[i];
	    }
/* L4: */
	}
	if (i1 >= kk) {
	    ++i1;
	}
	if (i2 >= kk) {
	    ++i2;
	}
	if (i3 >= kk) {
	    ++i3;
	}
    }

/* Insert K into X and Y, and update LIST, LPTR, LEND, and */
/*   LNEW with the arcs containing node K. */

    x[kk] = *xk;
    y[kk] = *yk;
    if (i3 == 0) {
	bdyadd_(&kk, &i1, &i2, &list[1], &lptr[1], &lend[1], lnew);
    } else {
	intadd_(&kk, &i1, &i2, &i3, &list[1], &lptr[1], &lend[1], lnew);
    }

/* Initialize variables for optimization of the triangula- */
/*   tion. */

    lp = lend[kk];
    lpf = lptr[lp];
    io2 = list[lpf];
    lpo1 = lptr[lpf];
    io1 = (i__1 = list[lpo1], abs(i__1));

/* Begin loop:  find the node opposite K. */

L5:
    lp = lstptr_(&lend[io1], &io2, &list[1], &lptr[1]);
    if (list[lp] < 0) {
	goto L6;
    }
    lp = lptr[lp];
    in1 = (i__1 = list[lp], abs(i__1));
    if (crtri_(ncc, &lcc[1], &io1, &io2, &in1)) {
	goto L6;
    }

/* Swap test:  if a swap occurs, two new arcs are */
/*             opposite K and must be tested. */

    if (! swptst_(&in1, &kk, &io1, &io2, &x[1], &y[1])) {
	goto L6;
    }
    swap_(&in1, &kk, &io1, &io2, &list[1], &lptr[1], &lend[1], &lpo1);
    io1 = in1;
    goto L5;

/* No swap occurred.  Test for termination and reset */
/*   IO2 and IO1. */

L6:
    if (lpo1 == lpf || list[lpo1] < 0) {
	return 0;
    }
    io2 = io1;
    lpo1 = lptr[lpo1];
    io1 = (i__1 = list[lpo1], abs(i__1));
    goto L5;

/* A parameter is outside its valid range on input. */

L7:
    *ier = -1;
    return 0;

/* All nodes are collinear. */

L8:
    *ier = -2;
    return 0;

/* Nodes L and K coincide. */

L9:
    *ier = l;
    return 0;

/* Node K lies in a constraint region. */

L10:
    *ier = -3;
    return 0;
} /* addnod_ */

/* Subroutine */ int bdyadd_(kk, i1, i2, list, lptr, lend, lnew)
integer *kk, *i1, *i2, *list, *lptr, *lend, *lnew;
{
    static integer lsav, nsav, next, k, n1, n2, lp;
    extern /* Subroutine */ int insert_();


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   02/22/91 */

/*   This subroutine adds a boundary node to a triangulation */
/* of a set of points in the plane.  The data structure is */
/* updated with the insertion of node KK, but no optimization */
/* is performed. */


/* On input: */

/*       KK = Index of a node to be connected to the sequence */
/*            of all visible boundary nodes.  KK .GE. 1 and */
/*            KK must not be equal to I1 or I2. */

/*       I1 = First (rightmost as viewed from KK) boundary */
/*            node in the triangulation which is visible from */
/*            node KK (the line segment KK-I1 intersects no */
/*            arcs. */

/*       I2 = Last (leftmost) boundary node which is visible */
/*            from node KK.  I1 and I2 may be determined by */
/*            subroutine TRFIND. */

/* The above parameters are not altered by this routine. */

/*       LIST,LPTR,LEND,LNEW = Triangulation data structure */
/*                             created by TRMESH or TRMSHR. */
/*                             Nodes I1 and I2 must be in- */
/*                             cluded in the triangulation. */

/* On output: */

/*       LIST,LPTR,LEND,LNEW = Data structure updated with */
/*                             the addition of node KK.  Node */
/*                             KK is connected to I1, I2, and */
/*                             all boundary nodes in between. */

/* Module required by BDYADD:  INSERT */

/* *********************************************************** */

    /* Parameter adjustments */
    --lend;
    --lptr;
    --list;

    /* Function Body */
    k = *kk;
    n1 = *i1;
    n2 = *i2;

/* Add K as the last neighbor of N1. */

    lp = lend[n1];
    lsav = lptr[lp];
    lptr[lp] = *lnew;
    list[*lnew] = -k;
    lptr[*lnew] = lsav;
    lend[n1] = *lnew;
    ++(*lnew);
    next = -list[lp];
    list[lp] = next;
    nsav = next;

/* Loop on the remaining boundary nodes between N1 and N2, */
/*   adding K as the first neighbor. */

L1:
    lp = lend[next];
    insert_(&k, &lp, &list[1], &lptr[1], lnew);
    if (next == n2) {
	goto L2;
    }
    next = -list[lp];
    list[lp] = next;
    goto L1;

/* Add the boundary nodes between N1 and N2 as neighbors */
/*   of node K. */

L2:
    lsav = *lnew;
    list[*lnew] = n1;
    lptr[*lnew] = *lnew + 1;
    ++(*lnew);
    next = nsav;

L3:
    if (next == n2) {
	goto L4;
    }
    list[*lnew] = next;
    lptr[*lnew] = *lnew + 1;
    ++(*lnew);
    lp = lend[next];
    next = list[lp];
    goto L3;

L4:
    list[*lnew] = -n2;
    lptr[*lnew] = lsav;
    lend[k] = *lnew;
    ++(*lnew);
    return 0;
} /* bdyadd_ */

logical crtri_(ncc, lcc, i1, i2, i3)
integer *ncc, *lcc, *i1, *i2, *i3;
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    static integer imin, imax, i;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   08/14/91 */

/*   This function returns TRUE if and only if triangle (I1, */
/* I2,I3) lies in a constraint region. */


/* On input: */

/*       NCC,LCC = Constraint data structure.  Refer to sub- */
/*                 routine ADDCST. */

/*       I1,I2,I3 = Nodal indexes of the counterclockwise- */
/*                  ordered vertices of a triangle. */

/* Input parameters are altered by this function. */

/*       CRTRI = TRUE iff (I1,I2,I3) is a constraint region */
/*               triangle. */

/* Note that input parameters are not tested for validity. */

/* Modules required by CRTRI:  None */

/* Intrinsic functions called by CRTRI:  MAX, MIN */

/* *********************************************************** */

    /* Parameter adjustments */
    --lcc;

    /* Function Body */
/* Computing MAX */
    i__1 = max(*i1,*i2);
    imax = max(i__1,*i3);

/*   Find the index I of the constraint containing IMAX. */

    i = *ncc + 1;
L1:
    --i;
    if (i <= 0) {
	goto L2;
    }
    if (imax < lcc[i]) {
	goto L1;
    }
/* Computing MIN */
    i__1 = min(*i1,*i2);
    imin = min(i__1,*i3);

/* P lies in a constraint region iff I1, I2, and I3 are nodes */
/*   of the same constraint (IMIN >= LCC(I)), and (IMIN,IMAX) */
/*   is (I1,I3), (I2,I1), or (I3,I2). */

    ret_val = imin >= lcc[i] && (imin == *i1 && imax == *i3 || imin == *i2 && 
	    imax == *i1 || imin == *i3 && imax == *i2);
    return ret_val;

/* NCC .LE. 0 or all vertices are non-constraint nodes. */

L2:
    ret_val = FALSE_;
    return ret_val;
} /* crtri_ */

integer indxcc_(ncc, lcc, n, list, lend)
integer *ncc, *lcc, *n, *list, *lend;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer i, ilast, ifrst, n0, lp, nst, nxt;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   08/25/91 */

/*   Given a constrained Delaunay triangulation, this func- */
/* tion returns the index, if any, of an exterior constraint */
/* curve (an unbounded constraint region).  An exterior con- */
/* straint curve is assumed to be present if and only if the */
/* clockwise-ordered sequence of boundary nodes is a subse- */
/* quence of a constraint node sequence.  The triangulation */
/* adjacencies corresponding to constraint edges may or may */
/* not have been forced by a call to ADDCST, and the con- */
/* straint region may or may not be valid (contain no nodes). */


/* On input: */

/*       NCC = Number of constraints.  NCC .GE. 0. */

/*       LCC = List of constraint curve starting indexes (or */
/*             dummy array of length 1 if NCC = 0).  Refer to */
/*             subroutine ADDCST. */

/*       N = Number of nodes in the triangulation.  N .GE. 3. */

/*       LIST,LEND = Data structure defining the triangula- */
/*                   tion.  Refer to subroutine TRMESH. */

/*   Input parameters are not altered by this function.  Note */
/* that the parameters are not tested for validity. */

/* On output: */

/*       INDXCC = Index of the exterior constraint curve, if */
/*                present, or 0 otherwise. */

/* Modules required by INDXCC:  None */

/* *********************************************************** */

    /* Parameter adjustments */
    --lcc;
    --lend;
    --list;

    /* Function Body */
    ret_val = 0;
    if (*ncc < 1) {
	return ret_val;
    }

/* Set N0 to the boundary node with smallest index. */

    n0 = 0;
L1:
    ++n0;
    lp = lend[n0];
    if (list[lp] > 0) {
	goto L1;
    }

/* Search in reverse order for the constraint I, if any, that */
/*   contains N0.  IFRST and ILAST index the first and last */
/*   nodes in constraint I. */

    i = *ncc;
    ilast = *n;
L2:
    ifrst = lcc[i];
    if (n0 >= ifrst) {
	goto L3;
    }
    if (i == 1) {
	return ret_val;
    }
    --i;
    ilast = ifrst - 1;
    goto L2;

/* N0 is in constraint I which indexes an exterior constraint */
/*   curve iff the clockwise-ordered sequence of boundary */
/*   node indexes beginning with N0 is increasing and bounded */
/*   above by ILAST. */

L3:
    nst = n0;

L4:
    nxt = -list[lp];
    if (nxt == nst) {
	goto L5;
    }
    if (nxt <= n0 || nxt > ilast) {
	return ret_val;
    }
    n0 = nxt;
    lp = lend[n0];
    goto L4;

/* Constraint I contains the boundary node sequence as a */
/*   subset. */

L5:
    ret_val = i;
    return ret_val;
} /* indxcc_ */

/* Subroutine */ int insert_(k, lp, list, lptr, lnew)
integer *k, *lp, *list, *lptr, *lnew;
{
    static integer lsav;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   This subroutine inserts K as a neighbor of N1 following */
/* N2, where LP is the LIST pointer of N2 as a neighbor of */
/* N1.  Note that, if N2 is the last neighbor of N1, K will */
/* become the first neighbor (even if N1 is a boundary node). */


/* On input: */

/*       K = Index of the node to be inserted. */

/*       LP = LIST pointer of N2 as a neighbor of N1. */

/* The above parameters are not altered by this routine. */

/*       LIST,LPTR,LNEW = Data structure defining the trian- */
/*                        gulation.  Refer to subroutine */
/*                        TRMESH. */

/* On output: */

/*       LIST,LPTR,LNEW = Data structure updated with the */
/*                        addition of node K. */

/* Modules required by INSERT:  None */

/* *********************************************************** */


    /* Parameter adjustments */
    --lptr;
    --list;

    /* Function Body */
    lsav = lptr[*lp];
    lptr[*lp] = *lnew;
    list[*lnew] = *k;
    lptr[*lnew] = lsav;
    ++(*lnew);
    return 0;
} /* insert_ */

/* Subroutine */ int intadd_(kk, i1, i2, i3, list, lptr, lend, lnew)
integer *kk, *i1, *i2, *i3, *list, *lptr, *lend, *lnew;
{
    static integer k, n1, n2, n3, lp;
    extern /* Subroutine */ int insert_();
    extern integer lstptr_();


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   02/22/91 */

/*   This subroutine adds an interior node to a triangulation */
/* of a set of points in the plane.  The data structure is */
/* updated with the insertion of node KK into the triangle */
/* whose vertices are I1, I2, and I3.  No optimization of the */
/* triangulation is performed. */


/* On input: */

/*       KK = Index of the node to be inserted.  KK .GE. 1 */
/*            and KK must not be equal to I1, I2, or I3. */

/*       I1,I2,I3 = Indexes of the counterclockwise-ordered */
/*                  sequence of vertices of a triangle which */
/*                  contains node KK. */

/* The above parameters are not altered by this routine. */

/*       LIST,LPTR,LEND,LNEW = Data structure defining the */
/*                             triangulation.  Refer to sub- */
/*                             routine TRMESH.  Triangle */
/*                             (I1,I2,I3) must be included */
/*                             in the triangulation. */

/* On output: */

/*       LIST,LPTR,LEND,LNEW = Data structure updated with */
/*                             the addition of node KK.  KK */
/*                             will be connected to nodes I1, */
/*                             I2, and I3. */

/* Modules required by INTADD:  INSERT, LSTPTR */

/* *********************************************************** */

    /* Parameter adjustments */
    --lend;
    --lptr;
    --list;

    /* Function Body */
    k = *kk;

/* Initialization. */

    n1 = *i1;
    n2 = *i2;
    n3 = *i3;

/* Add K as a neighbor of I1, I2, and I3. */

    lp = lstptr_(&lend[n1], &n2, &list[1], &lptr[1]);
    insert_(&k, &lp, &list[1], &lptr[1], lnew);
    lp = lstptr_(&lend[n2], &n3, &list[1], &lptr[1]);
    insert_(&k, &lp, &list[1], &lptr[1], lnew);
    lp = lstptr_(&lend[n3], &n1, &list[1], &lptr[1]);
    insert_(&k, &lp, &list[1], &lptr[1], lnew);

/* Add I1, I2, and I3 as neighbors of K. */

    list[*lnew] = n1;
    list[*lnew + 1] = n2;
    list[*lnew + 2] = n3;
    lptr[*lnew] = *lnew + 1;
    lptr[*lnew + 1] = *lnew + 2;
    lptr[*lnew + 2] = *lnew;
    lend[k] = *lnew + 2;
    *lnew += 3;
    return 0;
} /* intadd_ */

logical intsec_(x1, y1, x2, y2, x3, y3, x4, y4)
doublereal *x1, *y1, *x2, *y2, *x3, *y3, *x4, *y4;
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static doublereal a, b, d, dx12, dx31, dy12, dy31, dx34, dy34;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   Given a pair of line segments P1-P2 and P3-P4, this */
/* function returns the value .TRUE. if and only if P1-P2 */
/* shares one or more points with P3-P4.  The line segments */
/* include their endpoints, and the four points need not be */
/* distinct.  Thus, either line segment may consist of a */
/* single point, and the segments may meet in a V (which is */
/* treated as an intersection).  Note that an incorrect */
/* decision may result from floating point error if the four */
/* endpoints are nearly collinear. */


/* On input: */

/*       X1,Y1 = Coordinates of P1. */

/*       X2,Y2 = Coordinates of P2. */

/*       X3,Y3 = Coordinates of P3. */

/*       X4,Y4 = Coordinates of P4. */

/* Input parameters are not altered by this function. */

/* On output: */

/*       INTSEC = Logical value defined above. */

/* Modules required by INTSEC:  None */

/* *********************************************************** */


/* Test for overlap between the smallest rectangles that */
/*   contain the line segments and have sides parallel to */
/*   the axes. */

    if (*x1 < *x3 && *x1 < *x4 && *x2 < *x3 && *x2 < *x4 || *x1 > *x3 && *x1 
	    > *x4 && *x2 > *x3 && *x2 > *x4 || *y1 < *y3 && *y1 < *y4 && *y2 <
	     *y3 && *y2 < *y4 || *y1 > *y3 && *y1 > *y4 && *y2 > *y3 && *y2 > 
	    *y4) {
	ret_val = FALSE_;
	return ret_val;
    }

/* Compute A = P4-P3 X P1-P3, B = P2-P1 X P1-P3, and */
/*   D = P2-P1 X P4-P3 (Z components). */

    dx12 = *x2 - *x1;
    dy12 = *y2 - *y1;
    dx34 = *x4 - *x3;
    dy34 = *y4 - *y3;
    dx31 = *x1 - *x3;
    dy31 = *y1 - *y3;
    a = dx34 * dy31 - dx31 * dy34;
    b = dx12 * dy31 - dx31 * dy12;
    d = dx12 * dy34 - dx34 * dy12;
    if (d == (float)0.) {
	goto L1;
    }

/* D .NE. 0 and the point of intersection of the lines de- */
/*   fined by the line segments is P = P1 + (A/D)*(P2-P1) = */
/*   P3 + (B/D)*(P4-P3). */

    ret_val = a / d >= (float)0. && a / d <= (float)1. && b / d >= (float)0. 
	    && b / d <= (float)1.;
    return ret_val;

/* D .EQ. 0 and thus either the line segments are parallel, */
/*   or one (or both) of them is a single point. */

L1:
    ret_val = a == (float)0. && b == (float)0.;
    return ret_val;
} /* intsec_ */

logical left_(x1, y1, x2, y2, x0, y0)
doublereal *x1, *y1, *x2, *y2, *x0, *y0;
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static doublereal dx1, dy1, dx2, dy2;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   This function determines whether node N0 is to the left */
/* or to the right of the line through N1-N2 as viewed by an */
/* observer at N1 facing N2. */


/* On input: */

/*       X1,Y1 = Coordinates of N1. */

/*       X2,Y2 = Coordinates of N2. */

/*       X0,Y0 = Coordinates of N0. */

/* Input parameters are not altered by this function. */

/* On output: */

/*       LEFT = .TRUE. if and only if (X0,Y0) is on or to the */
/*              left of the directed line N1->N2. */

/* Modules required by LEFT:  None */

/* *********************************************************** */


/* Local parameters: */

/* DX1,DY1 = X,Y components of the vector N1->N2 */
/* DX2,DY2 = X,Y components of the vector N1->N0 */

    dx1 = *x2 - *x1;
    dy1 = *y2 - *y1;
    dx2 = *x0 - *x1;
    dy2 = *y0 - *y1;

/* If the sign of the vector cross product of N1->N2 and */
/*   N1->N0 is positive, then sin(A) > 0, where A is the */
/*   angle between the vectors, and thus A is in the range */
/*   (0,180) degrees. */

    ret_val = dx1 * dy2 >= dx2 * dy1;
    return ret_val;
} /* left_ */

integer lstptr_(lpl, nb, list, lptr)
integer *lpl, *nb, *list, *lptr;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer nd, lp;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   This function returns the index (LIST pointer) of NB in */
/* the adjacency list for N0, where LPL = LEND(N0). */


/* On input: */

/*       LPL = LEND(N0) */

/*       NB = Index of the node whose pointer is to be re- */
/*            turned.  NB must be connected to N0. */

/*       LIST,LPTR = Data structure defining the triangula- */
/*                   tion.  Refer to subroutine TRMESH. */

/* Input parameters are not altered by this function. */

/* On output: */

/*       LSTPTR = Pointer such that LIST(LSTPTR) = NB or */
/*                LIST(LSTPTR) = -NB, unless NB is not a */
/*                neighbor of N0, in which case LSTPTR = LPL. */

/* Modules required by LSTPTR:  None */

/* *********************************************************** */


    /* Parameter adjustments */
    --lptr;
    --list;

    /* Function Body */
    lp = lptr[*lpl];
L1:
    nd = list[lp];
    if (nd == *nb) {
	goto L2;
    }
    lp = lptr[lp];
    if (lp != *lpl) {
	goto L1;
    }

L2:
    ret_val = lp;
    return ret_val;
} /* lstptr_ */

integer nbcnt_(lpl, lptr)
integer *lpl, *lptr;
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer k, lp;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   This function returns the number of neighbors of a node */
/* N0 in a triangulation created by subroutine TRMESH (or */
/* TRMSHR). */


/* On input: */

/*       LPL = LIST pointer to the last neighbor of N0 -- */
/*             LPL = LEND(N0). */

/*       LPTR = Array of pointers associated with LIST. */

/* Input parameters are not altered by this function. */

/* On output: */

/*       NBCNT = Number of neighbors of N0. */

/* Modules required by NBCNT:  None */

/* *********************************************************** */


    /* Parameter adjustments */
    --lptr;

    /* Function Body */
    lp = *lpl;
    k = 1;

L1:
    lp = lptr[lp];
    if (lp == *lpl) {
	goto L2;
    }
    ++k;
    goto L1;

L2:
    ret_val = k;
    return ret_val;
} /* nbcnt_ */

integer nearnd_(xp, yp, ist, n, x, y, list, lptr, lend, dsq)
doublereal *xp, *yp;
integer *ist, *n;
doublereal *x, *y;
integer *list, *lptr, *lend;
doublereal *dsq;
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1, d__2;

    /* Local variables */
    static integer l, i1, i2, i3, listp[25], n1, n2, lptrp[25], n3, lp, nn, 
	    nr;
    extern /* Subroutine */ int trfind_();
    static doublereal ds1;
    static integer lp1, lp2;
    extern integer lstptr_();
    static doublereal dx11, dx12, dx21, dx22, dy11, dy12, dy21, dy22;
    static integer lpl;
    static doublereal dsr;
    static integer nst;
    static doublereal cos1, cos2, sin1, sin2;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   10/31/90 */

/*   Given a point P in the plane and a Delaunay triangula- */
/* tion created by subroutine TRMESH or TRMSHR, this function */
/* returns the index of the nearest triangulation node to P. */

/*   The algorithm consists of implicitly adding P to the */
/* triangulation, finding the nearest neighbor to P, and */
/* implicitly deleting P from the triangulation.  Thus, it */
/* is based on the fact that, if P is a node in a Delaunay */
/* triangulation, the nearest node to P is a neighbor of P. */


/* On input: */

/*       XP,YP = Cartesian coordinates of the point P to be */
/*               located relative to the triangulation. */

/*       IST = Index of a node at which TRFIND begins the */
/*             search.  Search time depends on the proximity */
/*             of this node to P. */

/*       N = Number of nodes in the triangulation.  N .GE. 3. */

/*       X,Y = Arrays of length N containing the Cartesian */
/*             coordinates of the nodes. */

/*       LIST,LPTR,LEND = Data structure defining the trian- */
/*                        gulation.  Refer to TRMESH. */

/* Input parameters are not altered by this function. */

/* On output: */

/*       NEARND = Nodal index of the nearest node to P, or 0 */
/*                if N < 3 or the triangulation data struc- */
/*                ture is invalid. */

/*       DSQ = Squared distance between P and NEARND unless */
/*             NEARND = 0. */

/*       Note that the number of candidates for NEARND */
/*       (neighbors of P) is limited to LMAX defined in */
/*       the PARAMETER statement below. */

/* Modules required by NEARND:  LEFT, LSTPTR, TRFIND */

/* Intrinsic function called by NEARND:  ABS */

/* *********************************************************** */


/* Store local parameters and test for N invalid. */

    /* Parameter adjustments */
    --lend;
    --y;
    --x;
    --list;
    --lptr;

    /* Function Body */
    nn = *n;
    if (nn < 3) {
	goto L7;
    }
    nst = *ist;
    if (nst < 1 || nst > nn) {
	nst = 1;
    }

/* Find a triangle (I1,I2,I3) containing P, or the rightmost */
/*   (I1) and leftmost (I2) visible boundary nodes as viewed */
/*   from P. */

    trfind_(&nst, xp, yp, &x[1], &y[1], &list[1], &lptr[1], &lend[1], &i1, &
	    i2, &i3);

/* Test for collinear nodes. */

    if (i1 == 0) {
	goto L7;
    }

/* Store the linked list of 'neighbors' of P in LISTP and */
/*   LPTRP.  I1 is the first neighbor, and 0 is stored as */
/*   the last neighbor if P is not contained in a triangle. */
/*   L is the length of LISTP and LPTRP, and is limited to */
/*   LMAX. */

    if (i3 != 0) {
	listp[0] = i1;
	lptrp[0] = 2;
	listp[1] = i2;
	lptrp[1] = 3;
	listp[2] = i3;
	lptrp[2] = 1;
	l = 3;
    } else {
	n1 = i1;
	l = 1;
	lp1 = 2;
	listp[l - 1] = n1;
	lptrp[l - 1] = lp1;

/*   Loop on the ordered sequence of visible boundary nodes */
/*     N1 from I1 to I2. */

L1:
	lpl = lend[n1];
	n1 = -list[lpl];
	l = lp1;
	lp1 = l + 1;
	listp[l - 1] = n1;
	lptrp[l - 1] = lp1;
	if (n1 != i2 && lp1 < 25) {
	    goto L1;
	}
	l = lp1;
	listp[l - 1] = 0;
	lptrp[l - 1] = 1;
    }

/* Initialize variables for a loop on arcs N1-N2 opposite P */
/*   in which new 'neighbors' are 'swapped' in.  N1 follows */
/*   N2 as a neighbor of P, and LP1 and LP2 are the LISTP */
/*   indexes of N1 and N2. */

    lp2 = 1;
    n2 = i1;
    lp1 = lptrp[0];
    n1 = listp[lp1 - 1];

/* Begin loop:  find the node N3 opposite N1->N2. */

L2:
    lp = lstptr_(&lend[n1], &n2, &list[1], &lptr[1]);
    if (list[lp] < 0) {
	goto L4;
    }
    lp = lptr[lp];
    n3 = (i__1 = list[lp], abs(i__1));

/* Swap test:  Exit the loop if L = LMAX. */

    if (l == 25) {
	goto L5;
    }
    dx11 = x[n1] - x[n3];
    dx12 = x[n2] - x[n3];
    dx22 = x[n2] - *xp;
    dx21 = x[n1] - *xp;

    dy11 = y[n1] - y[n3];
    dy12 = y[n2] - y[n3];
    dy22 = y[n2] - *yp;
    dy21 = y[n1] - *yp;

    cos1 = dx11 * dx12 + dy11 * dy12;
    cos2 = dx22 * dx21 + dy22 * dy21;
    if (cos1 >= (float)0. && cos2 >= (float)0.) {
	goto L4;
    }
    if (cos1 < (float)0. && cos2 < (float)0.) {
	goto L3;
    }

    sin1 = dx11 * dy12 - dx12 * dy11;
    sin2 = dx22 * dy21 - dx21 * dy22;
    if (sin1 * cos2 + cos1 * sin2 >= (float)0.) {
	goto L4;
    }

/* Swap:  Insert N3 following N2 in the adjacency list for P. */
/*        The two new arcs opposite P must be tested. */

L3:
    ++l;
    lptrp[lp2 - 1] = l;
    listp[l - 1] = n3;
    lptrp[l - 1] = lp1;
    lp1 = l;
    n1 = n3;
    goto L2;

/* No swap:  Advance to the next arc and test for termination */
/*           on N1 = I1 (LP1 = 1) or N1 followed by 0. */

L4:
    if (lp1 == 1) {
	goto L5;
    }
    lp2 = lp1;
    n2 = n1;
    lp1 = lptrp[lp1 - 1];
    n1 = listp[lp1 - 1];
    if (n1 == 0) {
	goto L5;
    }
    goto L2;

/* Set NR and DSR to the index of the nearest node to P and */
/*   its squared distance from P, respectively. */

L5:
    nr = i1;
/* Computing 2nd power */
    d__1 = x[nr] - *xp;
/* Computing 2nd power */
    d__2 = y[nr] - *yp;
    dsr = d__1 * d__1 + d__2 * d__2;
    i__1 = l;
    for (lp = 2; lp <= i__1; ++lp) {
	n1 = listp[lp - 1];
	if (n1 == 0) {
	    goto L6;
	}
/* Computing 2nd power */
	d__1 = x[n1] - *xp;
/* Computing 2nd power */
	d__2 = y[n1] - *yp;
	ds1 = d__1 * d__1 + d__2 * d__2;
	if (ds1 < dsr) {
	    nr = n1;
	    dsr = ds1;
	}
L6:
	;
    }
    *dsq = dsr;
    ret_val = nr;
    return ret_val;

/* Invalid input. */

L7:
    ret_val = 0;
    return ret_val;
} /* nearnd_ */

doublereal store_(x)
doublereal *x;
{
    /* System generated locals */
    doublereal ret_val;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   03/18/90 */

/*   This function forces its argument X to be stored in a */
/* memory location, thus providing a means of determining */
/* floating point number characteristics (such as the machine */
/* precision) when it is necessary to avoid computation in */
/* high precision registers. */


/* On input: */

/*       X = Value to be stored. */

/* X is not altered by this function. */

/* On output: */

/*       STORE = Value of X after it has been stored and */
/*               possibly truncated or rounded to the single */
/*               precision word length. */

/* Modules required by STORE:  None */

/* *********************************************************** */


    stcom_1.y = *x;
    ret_val = stcom_1.y;
    return ret_val;
} /* store_ */

/* Subroutine */ int swap_(in1, in2, io1, io2, list, lptr, lend, lp21)
integer *in1, *in2, *io1, *io2, *list, *lptr, *lend, *lp21;
{
    static integer lpsav, lp;
    extern integer lstptr_();
    static integer lph;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   Given a triangulation of a set of points in the plane, */
/* this subroutine replaces a diagonal arc in a strictly */
/* convex quadrilateral (defined by a pair of adjacent tri- */
/* angles) with the other diagonal. */


/* On input: */

/*       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of */
/*                         the quadrilateral.  IO1-IO2 is re- */
/*                         placed by IN1-IN2.  (IO1,IO2,IN1) */
/*                         and (IO2,IO1,IN2) must be trian- */
/*                         gles on input. */

/* The above parameters are not altered by this routine. */

/*       LIST,LPTR,LEND = Data structure defining the trian- */
/*                        gulation.  Refer to subroutine */
/*                        TRMESH. */

/* On output: */

/*       LIST,LPTR,LEND = Data structure updated with the */
/*                        swap -- triangles (IO1,IO2,IN1) and */
/*                        (IO2,IO1,IN2) are replaced by */
/*                        (IN1,IN2,IO2) and (IN2,IN1,IO1). */

/*       LP21 = Index of IN1 as a neighbor of IN2 after the */
/*              swap is performed. */

/* Module required by SWAP:  LSTPTR */

/* *********************************************************** */


/* Delete IO2 as a neighbor of IO1. */

    /* Parameter adjustments */
    --lend;
    --lptr;
    --list;

    /* Function Body */
    lp = lstptr_(&lend[*io1], in2, &list[1], &lptr[1]);
    lph = lptr[lp];
    lptr[lp] = lptr[lph];

/* If IO2 is the last neighbor of IO1, make IN2 the */
/*   last neighbor. */

    if (lend[*io1] == lph) {
	lend[*io1] = lp;
    }

/* Insert IN2 as a neighbor of IN1 following IO1 */
/*   using the hole created above. */

    lp = lstptr_(&lend[*in1], io1, &list[1], &lptr[1]);
    lpsav = lptr[lp];
    lptr[lp] = lph;
    list[lph] = *in2;
    lptr[lph] = lpsav;

/* Delete IO1 as a neighbor of IO2. */

    lp = lstptr_(&lend[*io2], in1, &list[1], &lptr[1]);
    lph = lptr[lp];
    lptr[lp] = lptr[lph];

/* If IO1 is the last neighbor of IO2, make IN1 the */
/*   last neighbor. */

    if (lend[*io2] == lph) {
	lend[*io2] = lp;
    }

/* Insert IN1 as a neighbor of IN2 following IO2. */

    lp = lstptr_(&lend[*in2], io2, &list[1], &lptr[1]);
    lpsav = lptr[lp];
    lptr[lp] = lph;
    list[lph] = *in1;
    lptr[lph] = lpsav;
    *lp21 = lph;
    return 0;
} /* swap_ */

logical swptst_(in1, in2, io1, io2, x, y)
integer *in1, *in2, *io1, *io2;
doublereal *x, *y;
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static doublereal sin12, dx11, dx12, dx22, dx21, dy11, dy12, dy22, dy21, 
	    cos1, cos2, sin1, sin2;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   09/01/88 */

/*   This function applies the circumcircle test to a quadri- */
/* lateral defined by a pair of adjacent triangles.  The */
/* diagonal arc (shared triangle side) should be swapped for */
/* the other diagonl if and only if the fourth vertex is */
/* strictly interior to the circumcircle of one of the */
/* triangles (the decision is independent of the choice of */
/* triangle).  Equivalently, the diagonal is chosen to maxi- */
/* mize the smallest of the six interior angles over the two */
/* pairs of possible triangles (the decision is for no swap */
/* if the quadrilateral is not strictly convex). */

/*   When the four vertices are nearly cocircular (the */
/* neutral case), the preferred decision is no swap -- in */
/* order to avoid unnecessary swaps and, more important, to */
/* avoid cycling in subroutine OPTIM which is called by */
/* DELNOD and EDGE.  Thus, a tolerance SWTOL (stored in */
/* SWPCOM by TRMESH or TRMSHR) is used to define 'nearness' */
/* to the neutral case. */


/* On input: */

/*       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of */
/*                         the quadrilateral.  IO1-IO2 is the */
/*                         triangulation arc (shared triangle */
/*                         side) to be replaced by IN1-IN2 if */
/*                         the decision is to swap.  The */
/*                         triples (IO1,IO2,IN1) and (IO2, */
/*                         IO1,IN2) must define triangles (be */
/*                         in counterclockwise order) on in- */
/*                         put. */

/*       X,Y = Arrays containing the nodal coordinates. */

/* Input parameters are not altered by this routine. */

/* On output: */

/*       SWPTST = .TRUE. if and only if the arc connecting */
/*                IO1 and IO2 is to be replaced. */

/* Modules required by SWPTST:  None */

/* *********************************************************** */


/* Tolerance stored by TRMESH or TRMSHR. */


/* Local parameters: */

/* DX11,DY11 = X,Y components of the vector IN1->IO1 */
/* DX12,DY12 = X,Y components of the vector IN1->IO2 */
/* DX22,DY22 = X,Y components of the vector IN2->IO2 */
/* DX21,DY21 = X,Y components of the vector IN2->IO1 */
/* SIN1 =      Cross product of the vectors IN1->IO1 and */
/*               IN1->IO2 -- proportional to sin(T1), where */
/*               T1 is the angle at IN1 formed by the vectors */
/* COS1 =      Inner product of the vectors IN1->IO1 and */
/*               IN1->IO2 -- proportional to cos(T1) */
/* SIN2 =      Cross product of the vectors IN2->IO2 and */
/*               IN2->IO1 -- proportional to sin(T2), where */
/*               T2 is the angle at IN2 formed by the vectors */
/* COS2 =      Inner product of the vectors IN2->IO2 and */
/*               IN2->IO1 -- proportional to cos(T2) */
/* SIN12 =     SIN1*COS2 + COS1*SIN2 -- proportional to */
/*               sin(T1+T2) */


/* Compute the vectors containing the angles T1 and T2. */

    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    dx11 = x[*io1] - x[*in1];
    dx12 = x[*io2] - x[*in1];
    dx22 = x[*io2] - x[*in2];
    dx21 = x[*io1] - x[*in2];

    dy11 = y[*io1] - y[*in1];
    dy12 = y[*io2] - y[*in1];
    dy22 = y[*io2] - y[*in2];
    dy21 = y[*io1] - y[*in2];

/* Compute inner products. */

    cos1 = dx11 * dx12 + dy11 * dy12;
    cos2 = dx22 * dx21 + dy22 * dy21;

/* The diagonals should be swapped iff (T1+T2) > 180 */
/*   degrees.  The following two tests ensure numerical */
/*   stability:  the decision must be FALSE when both */
/*   angles are close to 0, and TRUE when both angles */
/*   are close to 180 degrees. */

    if (cos1 >= (float)0. && cos2 >= (float)0.) {
	goto L2;
    }
    if (cos1 < (float)0. && cos2 < (float)0.) {
	goto L1;
    }

/* Compute vector cross products (Z-components). */

    sin1 = dx11 * dy12 - dx12 * dy11;
    sin2 = dx22 * dy21 - dx21 * dy22;
    sin12 = sin1 * cos2 + cos1 * sin2;
    if (sin12 >= -swpcom_1.swtol) {
	goto L2;
    }

/* Swap. */

L1:
    ret_val = TRUE_;
    return ret_val;

/* No swap. */

L2:
    ret_val = FALSE_;
    return ret_val;
} /* swptst_ */

/* Subroutine */ int trfind_(nst, px, py, x, y, list, lptr, lend, i1, i2, i3)
integer *nst;
doublereal *px, *py, *x, *y;
integer *list, *lptr, *lend, *i1, *i2, *i3;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern logical left_();
    static integer n0, n1, n2, n3, n4, nb, nf, nl, lp, np;
    static doublereal xp, yp;
    extern integer lstptr_();
    static integer npp;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   06/14/90 */

/*   This subroutine locates a point P relative to a triangu- */
/* lation created by subroutine TRMESH or TRMSHR.  If P is */
/* contained in a triangle, the three vertex indexes are */
/* returned.  Otherwise, the indexes of the rightmost and */
/* leftmost visible boundary nodes are returned. */


/* On input: */

/*       NST = Index of a node at which TRFIND begins the */
/*             search.  Search time depends on the proximity */
/*             of this node to P. */

/*       PX,PY = X and Y coordinates of the point P to be */
/*               located. */

/*       X,Y = Arrays containing the coordinates of the nodes */
/*             in the triangulation. */

/*       LIST,LPTR,LEND = Data structure defining the trian- */
/*                        gulation.  Refer to subroutine */
/*                        TRMESH. */

/* Input parameters are not altered by this routine. */

/* On output: */

/*       I1,I2,I3 = Nodal indexes, in counterclockwise order, */
/*                  of the vertices of a triangle containing */
/*                  P, or, if P is not contained in the con- */
/*                  vex hull of the nodes, I1 indexes the */
/*                  rightmost visible boundary node, I2 in- */
/*                  dexes the leftmost visible boundary node, */
/*                  and I3 = 0.  Rightmost and leftmost are */
/*                  defined from the perspective of P, and a */
/*                  pair of points are visible from each */
/*                  other if and only if the line segment */
/*                  joining them intersects no triangulation */
/*                  arc.  If P and all of the nodes lie on a */
/*                  common line, then I1 = I2 = I3 = 0 on */
/*                  output. */

/* Modules required by TRFIND:  LEFT, LSTPTR */

/* Intrinsic functions called by TRFIND:  ABS, MAX */

/* *********************************************************** */


/* FRWRD = TRUE iff C is forward of A->B */
/*              iff <A->B,A->C> .GE. 0. */


    /* Parameter adjustments */
    --lend;
    --lptr;
    --list;
    --y;
    --x;

    /* Function Body */
    n0 = max(*nst,1);
    xp = *px;
    yp = *py;

/* Set N1 = NF and NL to the first and last neighbors of N0. */

L1:
    lp = lend[n0];
    nl = list[lp];
    lp = lptr[lp];
    nf = list[lp];
    n1 = nf;

/* Find a pair of adjacent neighbors N1,N2 of N0 which define */
/*   a wedge containing P:  P LEFT N0->N1 and P RIGHT N0->N2. */

    if (nl > 0) {
	goto L2;
    }

/*   N0 is a boundary node.  Test for P exterior. */

    nl = -nl;
    if (! left_(&x[n0], &y[n0], &x[nf], &y[nf], &xp, &yp)) {
	nl = n0;
	goto L9;
    }
    if (! left_(&x[nl], &y[nl], &x[n0], &y[n0], &xp, &yp)) {
	nb = nf;
	nf = n0;
	np = nl;
	npp = n0;
	goto L11;
    }
    goto L3;

/*   N0 is an interior node.  Find N1. */

L2:
    if (left_(&x[n0], &y[n0], &x[n1], &y[n1], &xp, &yp)) {
	goto L3;
    }
    lp = lptr[lp];
    n1 = list[lp];
    if (n1 == nl) {
	goto L6;
    }
    goto L2;

/*   P is to the left of edge N0->N1.  Initialize N2 to the */
/*     next neighbor of N0. */

L3:
    lp = lptr[lp];
    n2 = (i__1 = list[lp], abs(i__1));
    if (! left_(&x[n0], &y[n0], &x[n2], &y[n2], &xp, &yp)) {
	goto L7;
    }
    n1 = n2;
    if (n1 != nl) {
	goto L3;
    }
    if (! left_(&x[n0], &y[n0], &x[nf], &y[nf], &xp, &yp)) {
	goto L6;
    }
    if (xp == x[n0] && yp == y[n0]) {
	goto L5;
    }

/*   P is left of or on edges N0->NB for all neighbors NB */
/*     of N0. */
/*   All points are collinear iff P is left of NB->N0 for */
/*     all neighbors NB of N0.  Search the neighbors of N0. */
/*     NOTE -- N1 = NL and LP points to NL. */

L4:
    if (! left_(&x[n1], &y[n1], &x[n0], &y[n0], &xp, &yp)) {
	goto L5;
    }
    lp = lptr[lp];
    n1 = (i__1 = list[lp], abs(i__1));
    if (n1 == nl) {
	goto L17;
    }
    goto L4;

/*   P is to the right of N1->N0, or P=N0.  Set N0 to N1 and */
/*     start over. */

L5:
    n0 = n1;
    goto L1;

/*   P is between edges N0->N1 and N0->NF. */

L6:
    n2 = nf;

/* P is contained in the wedge defined by line segments */
/*   N0->N1 and N0->N2, where N1 is adjacent to N2.  Set */
/*   N3 to the node opposite N1->N2. */

L7:
    n3 = n0;

/* Top of edge hopping loop.  Test for termination. */

L8:
    if (left_(&x[n1], &y[n1], &x[n2], &y[n2], &xp, &yp)) {

/*   P LEFT N1->N2 and hence P is in (N1,N2,N3) unless an */
/*     error resulted from floating point inaccuracy and */
/*     collinearity. */

	if (left_(&x[n2], &y[n2], &x[n3], &y[n3], &xp, &yp) && left_(&x[n3], &
		y[n3], &x[n1], &y[n1], &xp, &yp)) {
	    goto L16;
	}
    }

/*   Set N4 to the neighbor of N2 which follows N1 (node */
/*     opposite N2->N1) unless N1->N2 is a boundary edge. */

    lp = lstptr_(&lend[n2], &n1, &list[1], &lptr[1]);
    if (list[lp] < 0) {
	nf = n2;
	nl = n1;
	goto L9;
    }
    lp = lptr[lp];
    n4 = (i__1 = list[lp], abs(i__1));

/*   Select the new edge N1->N2 which intersects the line */
/*     segment N0-P, and set N3 to the node opposite N1->N2. */

    if (left_(&x[n0], &y[n0], &x[n4], &y[n4], &xp, &yp)) {
	n3 = n1;
	n1 = n4;
    } else {
	n3 = n2;
	n2 = n4;
    }
    goto L8;

/* Boundary traversal loops.  NL->NF is a boundary edge and */
/*   P RIGHT NL->NF.  Save NL and NF. */
L9:
    np = nl;
    npp = nf;

/* Find the first (rightmost) visible boundary node NF.  NB */
/*   is set to the first neighbor of NF, and NP is the last */
/*   neighbor. */

L10:
    lp = lend[nf];
    lp = lptr[lp];
    nb = list[lp];
    if (! left_(&x[nf], &y[nf], &x[nb], &y[nb], &xp, &yp)) {
	goto L12;
    }

/*   P LEFT NF->NB and thus NB is not visible unless an error */
/*     resulted from floating point inaccuracy and collinear- */
/*     ity of the 4 points NP, NF, NB, and P. */

L11:
    if ((x[np] - x[nf]) * (xp - x[nf]) + (y[np] - y[nf]) * (yp - y[nf]) >= (
	    float)0. || (x[np] - x[nf]) * (x[nb] - x[nf]) + (y[np] - y[nf]) * 
	    (y[nb] - y[nf]) >= (float)0.) {
	*i1 = nf;
	goto L13;
    }

/*   Bottom of loop. */

L12:
    np = nf;
    nf = nb;
    goto L10;

/* Find the last (leftmost) visible boundary node NL.  NB */
/*   is set to the last neighbor of NL, and NPP is the first */
/*   neighbor. */

L13:
    lp = lend[nl];
    nb = -list[lp];
    if (! left_(&x[nb], &y[nb], &x[nl], &y[nl], &xp, &yp)) {
	goto L14;
    }

/*   P LEFT NB->NL and thus NB is not visible unless an error */
/*     resulted from floating point inaccuracy and collinear- */
/*     ity of the 4 points P, NB, NL, and NPP. */

    if ((x[npp] - x[nl]) * (xp - x[nl]) + (y[npp] - y[nl]) * (yp - y[nl]) >= (
	    float)0. || (x[npp] - x[nl]) * (x[nb] - x[nl]) + (y[npp] - y[nl]) 
	    * (y[nb] - y[nl]) >= (float)0.) {
	goto L15;
    }

/*   Bottom of loop. */

L14:
    npp = nl;
    nl = nb;
    goto L13;

/* NL is the leftmost visible boundary node. */

L15:
    *i2 = nl;
    *i3 = 0;
    return 0;

/* P is in the triangle (N1,N2,N3). */

L16:
    *i1 = n1;
    *i2 = n2;
    *i3 = n3;
    return 0;

/* All points are collinear. */

L17:
    *i1 = 0;
    *i2 = 0;
    *i3 = 0;
    return 0;
} /* trfind_ */

/* Subroutine */ int trlist_(ncc, lcc, n, list, lptr, lend, nrow, nt, ltri, 
	lct, ier)
integer *ncc, *lcc, *n, *list, *lptr, *lend, *nrow, *nt, *ltri, *lct, *ier;
{
    /* System generated locals */
    integer ltri_dim1, ltri_offset, i__1, i__2;

    /* Local variables */
    static logical arcs;
    static integer lpln1;
    static logical pass2;
    static integer i, j, l, jlast;
    static logical cstri;
    static integer i1, i2, i3, n1, n2, n3, ka, kn, lp, nn, kt, nm2, lp2, lpl, 
	    isv, lcc1, n1st;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   11/12/94 */

/*   This subroutine converts a triangulation data structure */
/* from the linked list created by subroutine TRMESH or */
/* TRMSHR to a triangle list. */

/* On input: */

/*       NCC = Number of constraints.  NCC .GE. 0. */

/*       LCC = List of constraint curve starting indexes (or */
/*             dummy array of length 1 if NCC = 0).  Refer to */
/*             subroutine ADDCST. */

/*       N = Number of nodes in the triangulation.  N .GE. 3. */

/*       LIST,LPTR,LEND = Linked list data structure defin- */
/*                        ing the triangulation.  Refer to */
/*                        subroutine TRMESH. */

/*       NROW = Number of rows (entries per triangle) re- */
/*              served for the triangle list LTRI.  The value */
/*              must be 6 if only the vertex indexes and */
/*              neighboring triangle indexes are to be */
/*              stored, or 9 if arc indexes are also to be */
/*              assigned and stored.  Refer to LTRI. */

/* The above parameters are not altered by this routine. */

/*       LTRI = Integer array of length at least NROW*NT, */
/*              where NT is at most 2N-5.  (A sufficient */
/*              length is 12N if NROW=6 or 18N if NROW=9.) */

/*       LCT = Integer array of length NCC or dummy array of */
/*             length 1 if NCC = 0. */

/* On output: */

/*       NT = Number of triangles in the triangulation unless */
/*            IER .NE. 0, in which case NT = 0.  NT = 2N - NB */
/*            - 2, where NB is the number of boundary nodes. */

/*       LTRI = NROW by NT array whose J-th column contains */
/*              the vertex nodal indexes (first three rows), */
/*              neighboring triangle indexes (second three */
/*              rows), and, if NROW = 9, arc indexes (last */
/*              three rows) associated with triangle J for */
/*              J = 1,...,NT.  The vertices are ordered */
/*              counterclockwise with the first vertex taken */
/*              to be the one with smallest index.  Thus, */
/*              LTRI(2,J) and LTRI(3,J) are larger than */
/*              LTRI(1,J) and index adjacent neighbors of */
/*              node LTRI(1,J).  For I = 1,2,3, LTRI(I+3,J) */
/*              and LTRI(I+6,J) index the triangle and arc, */
/*              respectively, which are opposite (not shared */
/*              by) node LTRI(I,J), with LTRI(I+3,J) = 0 if */
/*              LTRI(I+6,J) indexes a boundary arc.  Vertex */
/*              indexes range from 1 to N, triangle indexes */
/*              from 0 to NT, and, if included, arc indexes */
/*              from 1 to NA = NT+N-1.  The triangles are or- */
/*              dered on first (smallest) vertex indexes, */
/*              except that the sets of constraint triangles */
/*              (triangles contained in the closure of a con- */
/*              straint region) follow the non-constraint */
/*              triangles. */

/*       LCT = Array of length NCC containing the triangle */
/*             index of the first triangle of constraint J in */
/*             LCT(J).  Thus, the number of non-constraint */
/*             triangles is LCT(1)-1, and constraint J con- */
/*             tains LCT(J+1)-LCT(J) triangles, where */
/*             LCT(NCC+1) = NT+1. */

/*       IER = Error indicator. */
/*             IER = 0 if no errors were encountered. */
/*             IER = 1 if NCC, N, NROW, or an LCC entry is */
/*                     outside its valid range on input. */
/*             IER = 2 if the triangulation data structure */
/*                     (LIST,LPTR,LEND) is invalid.  Note, */
/*                     however, that these arrays are not */
/*                     completely tested for validity. */

/* Modules required by TRLIST:  None */

/* Intrinsic function called by TRLIST:  ABS */

/* *********************************************************** */


/* Test for invalid input parameters and store the index */
/*   LCC1 of the first constraint node (if any). */

    /* Parameter adjustments */
    --lct;
    --lcc;
    --lend;
    --list;
    --lptr;
    ltri_dim1 = *nrow;
    ltri_offset = ltri_dim1 + 1;
    ltri -= ltri_offset;

    /* Function Body */
    nn = *n;
    if (*ncc < 0 || *nrow != 6 && *nrow != 9) {
	goto L12;
    }
    lcc1 = nn + 1;
    if (*ncc == 0) {
	if (nn < 3) {
	    goto L12;
	}
    } else {
	for (i = *ncc; i >= 1; --i) {
	    if (lcc1 - lcc[i] < 3) {
		goto L12;
	    }
	    lcc1 = lcc[i];
/* L1: */
	}
	if (lcc1 < 1) {
	    goto L12;
	}
    }

/* Initialize parameters for loop on triangles KT = (N1,N2, */
/*   N3), where N1 < N2 and N1 < N3.  This requires two */
/*   passes through the nodes with all non-constraint */
/*   triangles stored on the first pass, and the constraint */
/*   triangles stored on the second. */

/*   ARCS = TRUE iff arc indexes are to be stored. */
/*   KA,KT = Numbers of currently stored arcs and triangles. */
/*   N1ST = Starting index for the loop on nodes (N1ST = 1 on */
/*            pass 1, and N1ST = LCC1 on pass 2). */
/*   NM2 = Upper bound on candidates for N1. */
/*   PASS2 = TRUE iff constraint triangles are to be stored. */

    arcs = *nrow == 9;
    ka = 0;
    kt = 0;
    n1st = 1;
    nm2 = nn - 2;
    pass2 = FALSE_;

/* Loop on nodes N1:  J = constraint containing N1, */
/*                    JLAST = last node in constraint J. */

L2:
    j = 0;
    jlast = lcc1 - 1;
    i__1 = nm2;
    for (n1 = n1st; n1 <= i__1; ++n1) {
	if (n1 > jlast) {

/* N1 is the first node in constraint J+1.  Update J and */
/*   JLAST, and store the first constraint triangle index */
/*   if in pass 2. */

	    ++j;
	    if (j < *ncc) {
		jlast = lcc[j + 1] - 1;
	    } else {
		jlast = nn;
	    }
	    if (pass2) {
		lct[j] = kt + 1;
	    }
	}

/* Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points */
/*   to the last neighbor of N1, and LP2 points to N2. */

	lpln1 = lend[n1];
	lp2 = lpln1;
L3:
	lp2 = lptr[lp2];
	n2 = list[lp2];
	lp = lptr[lp2];
	n3 = (i__2 = list[lp], abs(i__2));
	if (n2 < n1 || n3 < n1) {
	    goto L10;
	}

/* (N1,N2,N3) is a constraint triangle iff the three nodes */
/*   are in the same constraint and N2 < N3.  Bypass con- */
/*   straint triangles on pass 1 and non-constraint triangles */
/*   on pass 2. */

	cstri = n1 >= lcc1 && n2 < n3 && n3 <= jlast;
	if (cstri && ! pass2 || ! cstri && pass2) {
	    goto L10;
	}

/* Add a new triangle KT = (N1,N2,N3). */

	++kt;
	ltri[kt * ltri_dim1 + 1] = n1;
	ltri[kt * ltri_dim1 + 2] = n2;
	ltri[kt * ltri_dim1 + 3] = n3;

/* Loop on triangle sides (I1,I2) with neighboring triangles */
/*   KN = (I1,I2,I3). */

	for (i = 1; i <= 3; ++i) {
	    if (i == 1) {
		i1 = n3;
		i2 = n2;
	    } else if (i == 2) {
		i1 = n1;
		i2 = n3;
	    } else {
		i1 = n2;
		i2 = n1;
	    }

/* Set I3 to the neighbor of I1 which follows I2 unless */
/*   I2->I1 is a boundary arc. */

	    lpl = lend[i1];
	    lp = lptr[lpl];
L4:
	    if (list[lp] == i2) {
		goto L5;
	    }
	    lp = lptr[lp];
	    if (lp != lpl) {
		goto L4;
	    }

/*   I2 is the last neighbor of I1 unless the data structure */
/*     is invalid.  Bypass the search for a neighboring */
/*     triangle if I2->I1 is a boundary arc. */

	    if ((i__2 = list[lp], abs(i__2)) != i2) {
		goto L13;
	    }
	    kn = 0;
	    if (list[lp] < 0) {
		goto L8;
	    }

/*   I2->I1 is not a boundary arc, and LP points to I2 as */
/*     a neighbor of I1. */

L5:
	    lp = lptr[lp];
	    i3 = (i__2 = list[lp], abs(i__2));

/* Find L such that LTRI(L,KN) = I3 (not used if KN > KT), */
/*   and permute the vertex indexes of KN so that I1 is */
/*   smallest. */

	    if (i1 < i2 && i1 < i3) {
		l = 3;
	    } else if (i2 < i3) {
		l = 2;
		isv = i1;
		i1 = i2;
		i2 = i3;
		i3 = isv;
	    } else {
		l = 1;
		isv = i1;
		i1 = i3;
		i3 = i2;
		i2 = isv;
	    }

/* Test for KN > KT (triangle index not yet assigned). */

	    if (i1 > n1 && ! pass2) {
		goto L9;
	    }

/* Find KN, if it exists, by searching the triangle list in */
/*   reverse order. */

	    for (kn = kt - 1; kn >= 1; --kn) {
		if (ltri[kn * ltri_dim1 + 1] == i1 && ltri[kn * ltri_dim1 + 2]
			 == i2 && ltri[kn * ltri_dim1 + 3] == i3) {
		    goto L7;
		}
/* L6: */
	    }
	    goto L9;

/* Store KT as a neighbor of KN. */

L7:
	    ltri[l + 3 + kn * ltri_dim1] = kt;

/* Store KN as a neighbor of KT, and add a new arc KA. */

L8:
	    ltri[i + 3 + kt * ltri_dim1] = kn;
	    if (arcs) {
		++ka;
		ltri[i + 6 + kt * ltri_dim1] = ka;
		if (kn != 0) {
		    ltri[l + 6 + kn * ltri_dim1] = ka;
		}
	    }
L9:
	    ;
	}

/* Bottom of loop on triangles. */

L10:
	if (lp2 != lpln1) {
	    goto L3;
	}
/* L11: */
    }

/* Bottom of loop on nodes. */

    if (! pass2 && *ncc > 0) {
	pass2 = TRUE_;
	n1st = lcc1;
	goto L2;
    }

/* No errors encountered. */

    *nt = kt;
    *ier = 0;
    return 0;

/* Invalid input parameter. */

L12:
    *nt = 0;
    *ier = 1;
    return 0;

/* Invalid triangulation data structure:  I1 is a neighbor of */
/*   I2, but I2 is not a neighbor of I1. */

L13:
    *nt = 0;
    *ier = 2;
    return 0;
} /* trlist_ */

/* Subroutine */ int trmesh_(n, x, y, list, lptr, lend, lnew, ier)
integer *n;
doublereal *x, *y;
integer *list, *lptr, *lend, *lnew, *ier;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Local variables */
    extern logical left_();
    static integer k;
    extern doublereal store_();
    extern /* Subroutine */ int addnod_();
    static integer nn, km1, lcc[1], ncc;
    static doublereal eps;


/* *********************************************************** */

/*                                               From TRIPACK */
/*             in memory locations rather than registers is */
/*             computed. */

/*  TRLIST - Converts the triangulation data structure to a */
/*             triangle list more suitable for use in a fin- */
/*             ite element code. */

/*  TRLPRT - Prints the triangle list created by subroutine */
/*             TRLIST. */

/*  TRMESH - Creates a Delaunay triangulation of a set of */
/*             nodes. */

/*  TRMSHR - Creates a Delaunay triangulation (more effici- */
/*             ently than TRMESH) of a set of nodes lying at */
/*             the vertices of a (possibly skewed) rectangu- */
/*             lar grid. */

/*  TRPRNT - Prints the triangulation data structure and, */
/*             optionally, the nodal coordinates. */


/* On input: */

/*       N = Number of nodes in the triangulation.  N .GE. 3. */

/*       X,Y = Arrays of length N containing the Cartesian */
/*             coordinates of the nodes.  (X(K),Y(K)) is re- */
/*             ferred to as node K, and K is referred to as */
/*             a nodal index.  The first three nodes must not */
/*             be collinear. */

/* The above parameters are not altered by this routine. */

/*       LIST,LPTR = Arrays of length at least 6N-12. */

/*       LEND = Array of length at least N. */

/* On output: */

/*       LIST = Set of nodal indexes which, along with LPTR, */
/*              LEND, and LNEW, define the triangulation as a */
/*              set of N adjacency lists -- counterclockwise- */
/*              ordered sequences of neighboring nodes such */
/*              that the first and last neighbors of a bound- */
/*              ary node are boundary nodes (the first neigh- */
/*              bor of an interior node is arbitrary).  In */
/*              order to distinguish between interior and */
/*              boundary nodes, the last neighbor of each */
/*              boundary node is represented by the negative */
/*              of its index. */

/*       LPTR = Set of pointers (LIST indexes) in one-to-one */
/*              correspondence with the elements of LIST. */
/*              LIST(LPTR(I)) indexes the node which follows */
/*              LIST(I) in cyclical counterclockwise order */
/*              (the first neighbor follows the last neigh- */
/*              bor). */

/*       LEND = Set of pointers to adjacency lists.  LEND(K) */
/*              points to the last neighbor of node K for */
/*              K = 1,...,N.  Thus, LIST(LEND(K)) < 0 if and */
/*              only if K is a boundary node. */

/*       LNEW = Pointer to the first empty location in LIST */
/*              and LPTR (list length plus one).  LIST, LPTR, */
/*              LEND, and LNEW are not altered if IER < 0, */
/*              and are incomplete if IER > 0. */

/*       IER = Error indicator: */
/*             IER =  0 if no errors were encountered. */
/*             IER = -1 if N < 3 on input. */
/*             IER = -2 if the first three nodes are */
/*                      collinear. */
/*             IER =  L if nodes L and M coincide for some */
/*                      M > L.  The linked list represents */
/*                      a triangulation of nodes 1 to M-1 */
/*                      in this case. */

/* Modules required by TRMESH:  ADDNOD, BDYADD, INSERT, */
/*                                INTADD, LEFT, LSTPTR, */
/*                                STORE, SWAP, SWPTST, TRFIND */

/* *********************************************************** */


    /* Parameter adjustments */
    --lend;
    --y;
    --x;
    --list;
    --lptr;

    /* Function Body */
    nn = *n;
    if (nn < 3) {
	*ier = -1;
	return 0;
    }

/* Compute a tolerance for function SWPTST:  SWTOL = 10* */
/*   (machine precision) */

    eps = (float)1.;
L1:
    eps /= (float)2.;
    d__1 = eps + (float)1.;
    swpcom_1.swtol = store_(&d__1);
    if (swpcom_1.swtol > (float)1.) {
	goto L1;
    }
    swpcom_1.swtol = eps * (float)20.;

/* Store the first triangle in the linked list. */

    if (! left_(&x[1], &y[1], &x[2], &y[2], &x[3], &y[3])) {

/*   The initial triangle is (1,3,2). */

	list[1] = 3;
	lptr[1] = 2;
	list[2] = -2;
	lptr[2] = 1;
	lend[1] = 2;

	list[3] = 1;
	lptr[3] = 4;
	list[4] = -3;
	lptr[4] = 3;
	lend[2] = 4;

	list[5] = 2;
	lptr[5] = 6;
	list[6] = -1;
	lptr[6] = 5;
	lend[3] = 6;

    } else if (! left_(&x[2], &y[2], &x[1], &y[1], &x[3], &y[3])) {

/*   The initial triangle is (1,2,3). */

	list[1] = 2;
	lptr[1] = 2;
	list[2] = -3;
	lptr[2] = 1;
	lend[1] = 2;

	list[3] = 3;
	lptr[3] = 4;
	list[4] = -1;
	lptr[4] = 3;
	lend[2] = 4;

	list[5] = 1;
	lptr[5] = 6;
	list[6] = -2;
	lptr[6] = 5;
	lend[3] = 6;

    } else {

/*   The first three nodes are collinear. */

	*ier = -2;
	return 0;
    }

/* Initialize LNEW and add the remaining nodes.  Parameters */
/*   for ADDNOD are as follows: */

/*   K = Index of the node to be added. */
/*   KM1 = Index of the starting node for the search in */
/*         TRFIND and number of nodes in the triangulation */
/*         on input. */
/*   NCC = Number of constraint curves. */
/*   LCC = Dummy array (since NCC = 0). */

    *lnew = 7;
    if (nn == 3) {
	*ier = 0;
	return 0;
    }
    ncc = 0;
    i__1 = nn;
    for (k = 4; k <= i__1; ++k) {
	km1 = k - 1;
	addnod_(&k, &x[k], &y[k], &km1, &ncc, lcc, &km1, &x[1], &y[1], &list[
		1], &lptr[1], &lend[1], lnew, ier);
	if (*ier != 0) {
	    return 0;
	}
/* L2: */
    }
    *ier = 0;
    return 0;
} /* trmesh_ */

/* Subroutine */ int trmshr_(n, nx, x, y, nit, list, lptr, lend, lnew, ier)
integer *n, *nx;
doublereal *x, *y;
integer *nit, *list, *lptr, *lend, *lnew, *ier;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1;

    /* Local variables */
    extern logical left_();
    static integer iter;
    extern /* Subroutine */ int swap_();
    static integer i, j, k;
    extern integer nbcnt_();
    static integer maxit;
    extern doublereal store_();
    static integer m1, m2, m3, m4, n0, n1, n2, n3, n4, ni, nj, lp, nn;
    extern /* Subroutine */ int insert_();
    static integer kp1, nm1;
    extern integer lstptr_();
    extern logical swptst_();
    static integer nnb, lpf, lpk, lpl;
    static doublereal eps;
    static integer lpp;
    static logical tst;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                      the allowable number of iterations. */
/*                      The triangulation is valid but not */
/*                      optimal in this case. */

/* Modules required by TRMSHR:  INSERT, LEFT, LSTPTR, NBCNT, */
/*                                STORE, SWAP, SWPTST */

/* Intrinsic function called by TRMSHR:  ABS */

/* *********************************************************** */


/* Store local variables and test for errors in input */
/*   parameters. */

    /* Parameter adjustments */
    --lend;
    --y;
    --x;
    --list;
    --lptr;

    /* Function Body */
    ni = *nx;
    nj = *n / ni;
    nn = ni * nj;
    maxit = *nit;
    *nit = 0;
    if (*n != nn || nj < 2 || ni < 2 || maxit < 0) {
	*ier = -1;
	return 0;
    }
    *ier = 0;

/* Compute a tolerance for function SWPTST:  SWTOL = 10* */
/*   (machine precision) */

    eps = (float)1.;
L1:
    eps /= (float)2.;
    d__1 = eps + (float)1.;
    swpcom_1.swtol = store_(&d__1);
    if (swpcom_1.swtol > (float)1.) {
	goto L1;
    }
    swpcom_1.swtol = eps * (float)20.;

/* Loop on grid points (I,J) corresponding to nodes K = */
/*   (J-1)*NI + I.  TST = TRUE iff diagonals are to be */
/*   chosen by the swap test.  M1, M2, M3, and M4 are the */
/*   slopes (-1, 0, or 1) of the diagonals in quadrants 1 */
/*   to 4 (counterclockwise beginning with the upper right) */
/*   for a coordinate system with origin at node K. */

    tst = maxit > 0;
    m1 = 0;
    m4 = 0;
    lp = 0;
    kp1 = 1;
    i__1 = nj;
    for (j = 1; j <= i__1; ++j) {
	i__2 = ni;
	for (i = 1; i <= i__2; ++i) {
	    m2 = m1;
	    m3 = m4;
	    k = kp1;
	    kp1 = k + 1;
	    lpf = lp + 1;
	    if (j == nj && i != ni) {
		goto L2;
	    }
	    if (i != 1) {
		if (j != 1) {

/*   K is not in the top row, leftmost column, or bottom r
ow */
/*     (unless K is the lower right corner).  Take the fir
st */
/*     neighbor to be the node above K. */

		    ++lp;
		    list[lp] = k - ni;
		    lptr[lp] = lp + 1;
		    if (m2 <= 0) {
			++lp;
			list[lp] = k - 1 - ni;
			lptr[lp] = lp + 1;
		    }
		}

/*   K is not in the leftmost column.  The next (or first) */
/*     neighbor is to the left of K. */

		++lp;
		list[lp] = k - 1;
		lptr[lp] = lp + 1;
		if (j == nj) {
		    goto L3;
		}
		if (m3 >= 0) {
		    ++lp;
		    list[lp] = k - 1 + ni;
		    lptr[lp] = lp + 1;
		}
	    }

/*   K is not in the bottom row.  The next (or first) */
/*     neighbor is below K. */

	    ++lp;
	    list[lp] = k + ni;
	    lptr[lp] = lp + 1;

/*   Test for a negative diagonal in quadrant 4 unless K is */
/*     in the rightmost column.  The quadrilateral associated */
/*     with the quadrant is tested for strict convexity un- */
/*     less NIT = 0 on input. */

	    if (i == ni) {
		goto L3;
	    }
	    m4 = 1;
	    if (! tst) {
		goto L2;
	    }
	    if (left_(&x[kp1], &y[kp1], &x[k + ni], &y[k + ni], &x[k], &y[k]) 
		    || left_(&x[k], &y[k], &x[kp1 + ni], &y[kp1 + ni], &x[k + 
		    ni], &y[k + ni]) || left_(&x[k + ni], &y[k + ni], &x[kp1],
		     &y[kp1], &x[kp1 + ni], &y[kp1 + ni]) || left_(&x[kp1 + 
		    ni], &y[kp1 + ni], &x[k], &y[k], &x[kp1], &y[kp1])) {
		goto L12;
	    }
	    i__3 = k + ni;
	    i__4 = kp1 + ni;
	    if (swptst_(&kp1, &i__3, &k, &i__4, &x[1], &y[1])) {
		goto L2;
	    }
	    m4 = -1;
	    ++lp;
	    list[lp] = kp1 + ni;
	    lptr[lp] = lp + 1;

/*   The next (or first) neighbor is to the right of K. */

L2:
	    ++lp;
	    list[lp] = kp1;
	    lptr[lp] = lp + 1;

/*   Test for a positive diagonal in quadrant 1 (the neighbor */
/*     of K-NI which follows K is not K+1) unless K is in the */
/*     top row. */

	    if (j == 1) {
		goto L3;
	    }
	    if (tst) {
		m1 = -1;
		lpk = lstptr_(&lend[k - ni], &k, &list[1], &lptr[1]);
		lpk = lptr[lpk];
		if (list[lpk] != kp1) {
		    m1 = 1;
		    ++lp;
		    list[lp] = kp1 - ni;
		    lptr[lp] = lp + 1;
		}
	    }

/*   If K is in the leftmost column (and not the top row) or */
/*     in the bottom row (and not the rightmost column), then */
/*     the next neighbor is the node above K. */

	    if (i != 1 && j != nj) {
		goto L4;
	    }
	    ++lp;
	    list[lp] = k - ni;
	    lptr[lp] = lp + 1;
	    if (i == 1) {
		goto L3;
	    }

/*   K is on the bottom row (and not the leftmost or right- */
/*     most column). */

	    if (m2 <= 0) {
		++lp;
		list[lp] = k - 1 - ni;
		lptr[lp] = lp + 1;
	    }
	    ++lp;
	    list[lp] = k - 1;
	    lptr[lp] = lp + 1;

/*   K is a boundary node. */

L3:
	    list[lp] = -list[lp];

/*   Bottom of loop.  Store LEND and correct LPTR(LP). */
/*     LPF and LP point to the first and last neighbors */
/*     of K. */

L4:
	    lend[k] = lp;
	    lptr[lp] = lpf;
/* L5: */
	}
/* L6: */
    }

/* Store LNEW, and terminate the algorithm if NIT = 0 on */
/*   input. */

    *lnew = lp + 1;
    if (maxit == 0) {
	return 0;
    }

/* Add boundary arcs where necessary in order to cover the */
/*   convex hull of the nodes.  N1, N2, and N3 are consecu- */
/*   tive boundary nodes in counterclockwise order, and N0 */
/*   is the starting point for each loop around the boundary. */

    n0 = 1;
    n1 = n0;
    n2 = ni + 1;

/*   TST is set to TRUE if an arc is added.  The boundary */
/*     loop is repeated until a traversal results in no */
/*     added arcs. */

L7:
    tst = FALSE_;

/*   Top of boundary loop.  Set N3 to the first neighbor of */
/*     N2, and test for N3 LEFT N1 -> N2. */

L8:
    lpl = lend[n2];
    lp = lptr[lpl];
    n3 = list[lp];
    if (left_(&x[n1], &y[n1], &x[n2], &y[n2], &x[n3], &y[n3])) {
	n1 = n2;
    }
    if (n1 != n2) {

/*   Add the boundary arc N1-N3.  If N0 = N2, the starting */
/*     point is changed to N3, since N2 will be removed from */
/*     the boundary.  N3 is inserted as the first neighbor of */
/*     N1, N2 is changed to an interior node, and N1 is */
/*     inserted as the last neighbor of N3. */

	tst = TRUE_;
	if (n2 == n0) {
	    n0 = n3;
	}
	lp = lend[n1];
	insert_(&n3, &lp, &list[1], &lptr[1], lnew);
	list[lpl] = -list[lpl];
	lp = lend[n3];
	list[lp] = n2;
	i__1 = -n1;
	insert_(&i__1, &lp, &list[1], &lptr[1], lnew);
	lend[n3] = *lnew - 1;
    }

/*   Bottom of loops.  Test for termination. */

    n2 = n3;
    if (n1 != n0) {
	goto L8;
    }
    if (tst) {
	goto L7;
    }

/* Terminate the algorithm if NIT = 1 on input. */

    *nit = 1;
    if (maxit == 1) {
	return 0;
    }

/* Optimize the triangulation by applying the swap test and */
/*   appropriate swaps to the interior arcs.  The loop is */
/*   repeated until no swaps are performed or MAXIT itera- */
/*   tions have been applied.  ITER is the current iteration, */
/*   and TST is set to TRUE if a swap occurs. */

    iter = 1;
    nm1 = nn - 1;
L9:
    ++iter;
    tst = FALSE_;

/*   Loop on interior arcs N1-N2, where N2 > N1 and */
/*     (N1,N2,N3) and (N2,N1,N4) are adjacent triangles. */

/*   Top of loop on nodes N1. */

    i__1 = nm1;
    for (n1 = 1; n1 <= i__1; ++n1) {
	lpl = lend[n1];
	n4 = list[lpl];
	lpf = lptr[lpl];
	n2 = list[lpf];
	lp = lptr[lpf];
	n3 = list[lp];
	nnb = nbcnt_(&lpl, &lptr[1]);

/*   Top of loop on neighbors N2 of N1.  NNB is the number of */
/*                                       neighbors of N1. */

	i__2 = nnb;
	for (i = 1; i <= i__2; ++i) {

/*   Bypass the swap test if N1 is a boundary node and N2 is */
/*     the first neighbor (N4 < 0), N2 < N1, or N1-N2 is a */
/*     diagonal arc (already locally optimal) when ITER = 2. */

	    if (n4 > 0 && n2 > n1 && (iter != 2 || (i__3 = n1 + ni - n2, abs(
		    i__3)) != 1)) {
		if (swptst_(&n3, &n4, &n1, &n2, &x[1], &y[1])) {

/*   Swap diagonal N1-N2 for N3-N4, set TST to TRUE, and s
et */
/*     N2 to N4 (the neighbor preceding N3). */

		    swap_(&n3, &n4, &n1, &n2, &list[1], &lptr[1], &lend[1], &
			    lpp);
		    tst = TRUE_;
		    n2 = n4;
		}
	    }

/*   Bottom of neighbor loop. */

	    if (list[lpl] == -n3) {
		goto L11;
	    }
	    n4 = n2;
	    n2 = n3;
	    lp = lstptr_(&lpl, &n2, &list[1], &lptr[1]);
	    lp = lptr[lp];
	    n3 = (i__3 = list[lp], abs(i__3));
/* L10: */
	}
L11:
	;
    }

/*   Test for termination. */

    if (tst && iter < maxit) {
	goto L9;
    }
    *nit = iter;
    if (tst) {
	*ier = -2;
    }
    return 0;

/* Invalid grid cell encountered. */

L12:
    *ier = k;
    return 0;
} /* trmshr_ */

/* Subroutine */ int trprnt_(ncc, lcc, n, x, y, list, lptr, lend, lout, prntx)
integer *ncc, *lcc, *n;
doublereal *x, *y;
integer *list, *lptr, *lend, *lout;
logical *prntx;
{
    /* Initialized data */

    static integer nmax = 9999;
    static integer nlmax = 60;

    /* Format strings */
    static char fmt_100[] = "(\0021\002,26x,\002ADJACENCY SETS,    N = \002,\
i5//)";
    static char fmt_110[] = "(1x,10x,\002*** N IS OUTSIDE ITS VALID\002,\002\
 RANGE ***\002)";
    static char fmt_101[] = "(1x,\002NODE\002,32x,\002NEIGHBORS OF NODE\002/\
/)";
    static char fmt_106[] = "(\0021\002)";
    static char fmt_103[] = "(1x,i4,5x,14i5/(1x,9x,14i5))";
    static char fmt_105[] = "(1x)";
    static char fmt_102[] = "(1x,\002NODE\002,5x,\002X(NODE)\002,8x,\002Y(NO\
DE)\002,20x,\002NEIGHBORS OF NODE\002//)";
    static char fmt_104[] = "(1x,i4,2e15.6,5x,8i5/(1x,39x,8i5))";
    static char fmt_107[] = "(/1x,\002NB = \002,i4,\002 BOUNDARY NODES\002,5\
x,\002NA = \002,i5,\002 ARCS\002,5x,\002NT = \002,i5,\002 TRIANGLES\002)";
    static char fmt_108[] = "(/1x,\002NCC =\002,i3,\002 CONSTRAINT CURVES\
\002)";
    static char fmt_109[] = "(1x,9x,14i5)";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static integer node, i, k, nabor[30], na, nb, nd, nl, lp, nn, nt, inc, 
	    lpl, lun;

    /* Fortran I/O blocks */
    static cilist io___177 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___178 = { 0, 0, 0, fmt_110, 0 };
    static cilist io___181 = { 0, 0, 0, fmt_101, 0 };
    static cilist io___189 = { 0, 0, 0, fmt_106, 0 };
    static cilist io___190 = { 0, 0, 0, fmt_103, 0 };
    static cilist io___192 = { 0, 0, 0, fmt_105, 0 };
    static cilist io___193 = { 0, 0, 0, fmt_102, 0 };
    static cilist io___194 = { 0, 0, 0, fmt_106, 0 };
    static cilist io___195 = { 0, 0, 0, fmt_104, 0 };
    static cilist io___196 = { 0, 0, 0, fmt_105, 0 };
    static cilist io___199 = { 0, 0, 0, fmt_106, 0 };
    static cilist io___200 = { 0, 0, 0, fmt_107, 0 };
    static cilist io___201 = { 0, 0, 0, fmt_108, 0 };
    static cilist io___202 = { 0, 0, 0, fmt_109, 0 };



/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   08/22/91 */

/*   Given a triangulation of a set of points in the plane, */
/* this subroutine prints the adjacency lists and, option- */
/* ally, the nodal coordinates on logical unit LOUT.  The */
/* list of neighbors of a boundary node is followed by index */
/* 0.  The numbers of boundary nodes, triangles, and arcs, */
/* and the constraint curve starting indexes, if any, are */
/* also printed. */


/* On input: */

/*       NCC = Number of constraints. */

/*       LCC = List of constraint curve starting indexes (or */
/*             dummy array of length 1 if NCC = 0). */

/*       N = Number of nodes in the triangulation. */
/*           3 .LE. N .LE. 9999. */

/*       X,Y = Arrays of length N containing the coordinates */
/*             of the nodes in the triangulation -- not used */
/*             unless PRNTX = TRUE. */

/*       LIST,LPTR,LEND = Data structure defining the trian- */
/*                        gulation.  Refer to subroutine */
/*                        TRMESH. */

/*       LOUT = Logical unit number for output.  0 .LE. LOUT */
/*              .LE. 99.  Output is printed on unit 6 if LOUT */
/*              is outside its valid range on input. */

/*       PRNTX = Logical variable with value TRUE if and only */
/*               if X and Y are to be printed (to 6 decimal */
/*               places). */

/* None of the parameters are altered by this routine. */

/* Modules required by TRPRNT:  None */

/* *********************************************************** */

    /* Parameter adjustments */
    --lcc;
    --lend;
    --y;
    --x;
    --list;
    --lptr;

    /* Function Body */

    nn = *n;
    lun = *lout;
    if (lun < 0 || lun > 99) {
	lun = 6;
    }

/* Print a heading and test the range of N. */

    io___177.ciunit = lun;
    s_wsfe(&io___177);
    do_fio(&c__1, (char *)&nn, (ftnlen)sizeof(integer));
    e_wsfe();
    if (nn < 3 || nn > nmax) {

/* N is outside its valid range. */

	io___178.ciunit = lun;
	s_wsfe(&io___178);
	e_wsfe();
	goto L5;
    }

/* Initialize NL (the number of lines printed on the current */
/*   page) and NB (the number of boundary nodes encountered). */

    nl = 6;
    nb = 0;
    if (! (*prntx)) {

/* Print LIST only.  K is the number of neighbors of NODE */
/*   which are stored in NABOR. */

	io___181.ciunit = lun;
	s_wsfe(&io___181);
	e_wsfe();
	i__1 = nn;
	for (node = 1; node <= i__1; ++node) {
	    lpl = lend[node];
	    lp = lpl;
	    k = 0;

L1:
	    ++k;
	    lp = lptr[lp];
	    nd = list[lp];
	    nabor[k - 1] = nd;
	    if (lp != lpl) {
		goto L1;
	    }
	    if (nd <= 0) {

/*   NODE is a boundary node.  Correct the sign of the last */
/*     neighbor, add 0 to the end of the list, and increment 
*/
/*     NB. */

		nabor[k - 1] = -nd;
		++k;
		nabor[k - 1] = 0;
		++nb;
	    }

/*   Increment NL and print the list of neighbors. */

	    inc = (k - 1) / 14 + 2;
	    nl += inc;
	    if (nl > nlmax) {
		io___189.ciunit = lun;
		s_wsfe(&io___189);
		e_wsfe();
		nl = inc;
	    }
	    io___190.ciunit = lun;
	    s_wsfe(&io___190);
	    do_fio(&c__1, (char *)&node, (ftnlen)sizeof(integer));
	    i__2 = k;
	    for (i = 1; i <= i__2; ++i) {
		do_fio(&c__1, (char *)&nabor[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	    if (k != 14) {
		io___192.ciunit = lun;
		s_wsfe(&io___192);
		e_wsfe();
	    }
/* L2: */
	}
    } else {

/* Print X, Y, and LIST. */

	io___193.ciunit = lun;
	s_wsfe(&io___193);
	e_wsfe();
	i__1 = nn;
	for (node = 1; node <= i__1; ++node) {
	    lpl = lend[node];
	    lp = lpl;
	    k = 0;
L3:
	    ++k;
	    lp = lptr[lp];
	    nd = list[lp];
	    nabor[k - 1] = nd;
	    if (lp != lpl) {
		goto L3;
	    }
	    if (nd <= 0) {

/*   NODE is a boundary node. */

		nabor[k - 1] = -nd;
		++k;
		nabor[k - 1] = 0;
		++nb;
	    }

/*   Increment NL and print X, Y, and NABOR. */

	    inc = (k - 1) / 8 + 2;
	    nl += inc;
	    if (nl > nlmax) {
		io___194.ciunit = lun;
		s_wsfe(&io___194);
		e_wsfe();
		nl = inc;
	    }
	    io___195.ciunit = lun;
	    s_wsfe(&io___195);
	    do_fio(&c__1, (char *)&node, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&x[node], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&y[node], (ftnlen)sizeof(doublereal));
	    i__2 = k;
	    for (i = 1; i <= i__2; ++i) {
		do_fio(&c__1, (char *)&nabor[i - 1], (ftnlen)sizeof(integer));
	    }
	    e_wsfe();
	    if (k != 8) {
		io___196.ciunit = lun;
		s_wsfe(&io___196);
		e_wsfe();
	    }
/* L4: */
	}
    }

/* Print NB, NA, and NT (boundary nodes, arcs, and */
/*   triangles). */

    nt = (nn << 1) - nb - 2;
    na = nt + nn - 1;
    if (nl > nlmax - 6) {
	io___199.ciunit = lun;
	s_wsfe(&io___199);
	e_wsfe();
    }
    io___200.ciunit = lun;
    s_wsfe(&io___200);
    do_fio(&c__1, (char *)&nb, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&na, (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&nt, (ftnlen)sizeof(integer));
    e_wsfe();

/* Print NCC and LCC. */

L5:
    io___201.ciunit = lun;
    s_wsfe(&io___201);
    do_fio(&c__1, (char *)&(*ncc), (ftnlen)sizeof(integer));
    e_wsfe();
    if (*ncc > 0) {
	io___202.ciunit = lun;
	s_wsfe(&io___202);
	i__1 = *ncc;
	for (i = 1; i <= i__1; ++i) {
	    do_fio(&c__1, (char *)&lcc[i], (ftnlen)sizeof(integer));
	}
	e_wsfe();
    }
    return 0;

/* Print formats: */

} /* trprnt_ */

/* Subroutine */ int troutp_(ncc, lcc, n, x, y, list, lptr, lend, lout, nabor,
	 na, nb, nt)
integer *ncc, *lcc, *n;
doublereal *x, *y;
integer *list, *lptr, *lend, *lout, *nabor, *na, *nb, *nt;
{
    /* Initialized data */

    static integer nmax = 9999;
    static integer nlmax = 60;

    /* System generated locals */
    integer nabor_dim1, nabor_offset, i__1;

    /* Local variables */
    static integer node, k, nd, nl, lp, nn, inc, lpl, lun;


/* *********************************************************** */

/*                                               From TRIPACK */
/*                                            Robert J. Renka */
/*                                  Dept. of Computer Science */
/*                                       Univ. of North Texas */
/*                                             (817) 565-2767 */
/*                                                   08/22/91 */

/*   modified version of TRPRNT */

/*   Given a triangulation of a set of points in the plane, */
/* this subroutine returns the adjacency lists and, option- */
/* ally, the nodal coordinates.  The */
/* list of neighbors of a boundary node is followed by index */
/* 0.  The numbers of boundary nodes, triangles, and arcs, */
/* and the constraint curve starting indexes, if any, are */
/* also returned. */


/* On input: */

/*       NCC = Number of constraints. */

/*       LCC = List of constraint curve starting indexes (or */
/*             dummy array of length 1 if NCC = 0). */

/*       N = Number of nodes in the triangulation. */
/*           3 .LE. N .LE. 9999. */

/*       X,Y = Arrays of length N containing the coordinates */
/*             of the nodes in the triangulation -- not used */
/*             unless PRNTX = TRUE. */

/*       LIST,LPTR,LEND = Data structure defining the trian- */
/*                        gulation.  Refer to subroutine */
/*                        TRMESH. */


/* None of these parameters are altered by this routine. */

/* Output */

/*     NABOR */
/*     NB */
/*     NA */
/*     NT */

/* Modules required by TRPRNT:  None */

/* *********************************************************** */

    /* Parameter adjustments */
    --lcc;
    nabor_dim1 = *n;
    nabor_offset = nabor_dim1 + 1;
    nabor -= nabor_offset;
    --lend;
    --y;
    --x;
    --list;
    --lptr;

    /* Function Body */

    nn = *n;
    lun = *lout;
    if (lun < 0 || lun > 99) {
	lun = 6;
    }

/* test the range of N. */

    if (nn < 3 || nn > nmax) {

/* N is outside its valid range. */

/*        WRITE (LUN,110) */
	goto L5;
    }

/* Initialize NL (the number of lines printed on the current */
/*   page) and NB (the number of boundary nodes encountered). */

    nl = 6;
    *nb = 0;

/* Print X, Y, and LIST. */

    i__1 = nn;
    for (node = 1; node <= i__1; ++node) {
	lpl = lend[node];
	lp = lpl;
	k = 0;
L3:
	++k;
	lp = lptr[lp];
	nd = list[lp];
	nabor[node + k * nabor_dim1] = nd;
	if (lp != lpl) {
	    goto L3;
	}
	if (nd <= 0) {

/*   NODE is a boundary node. */

	    nabor[node + k * nabor_dim1] = -nd;
	    ++k;
	    nabor[node + k * nabor_dim1] = 0;
	    ++(*nb);
	}

/*   Increment NL and print X, Y, and NABOR. */

	inc = (k - 1) / 8 + 2;
	nl += inc;
	if (nl > nlmax) {
	    nl = inc;
	}
/* L4: */
    }

/* Print NB, NA, and NT (boundary nodes, arcs, and */
/*   triangles). */

    *nt = (nn << 1) - *nb - 2;
    *na = *nt + nn - 1;

/* Print NCC and LCC. */

L5:
    return 0;

/* Print formats: */

/* L110: */
} /* troutp_ */

