C      ALGORITHM 751, COLLECTED ALGORITHMS FROM ACM.
C      THIS WORK PUBLISHED IN TRANSACTIONS ON MATHEMATICAL SOFTWARE,
C      VOL. 22, NO. 1, March, 1996, P.  1--8.
C      
C      This is not the complete tripack package!  
      SUBROUTINE ADDNOD (K,XK,YK,IST,NCC, LCC,N,X,Y,LIST,
     .                   LPTR,LEND,LNEW, IER)
      INTEGER K, IST, NCC, LCC(*), N, LIST(*), LPTR(*),
     .        LEND(*), LNEW, IER
      DOUBLE PRECISION    XK, YK, X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/25/91
C
C   Given a triangulation of N nodes in the plane created by
C subroutine TRMESH or TRMSHR, this subroutine updates the
C data structure with the addition of a new node in position
C K.  If node K is inserted into X and Y (K .LE. N) rather
C than appended (K = N+1), then a corresponding insertion
C must be performed in any additional arrays associated
C with the nodes.  For example, an array of data values Z
C must be shifted down to open up position K for the new
C value:  set Z(I+1) to Z(I) for I = N,N-1,...,K.  For
C optimal efficiency, new nodes should be appended whenever
C possible.  Insertion is necessary, however, to add a non-
C constraint node when constraints are present (refer to
C subroutine ADDCST).
C
C   Note that a constraint node cannot be added by this
C routine.  In order to insert a constraint node, it is
C necessary to add the node with no constraints present
C (call this routine with NCC = 0), update LCC by increment-
C ing the appropriate entries, and then create (or restore)
C the constraints by a call to ADDCST.
C
C   The algorithm consists of the following steps:  node K
C is located relative to the triangulation (TRFIND), its
C index is added to the data structure (INTADD or BDYADD),
C and a sequence of swaps (SWPTST and SWAP) are applied to
C the arcs opposite K so that all arcs incident on node K
C and opposite node K (excluding constraint arcs) are local-
C ly optimal (satisfy the circumcircle test).  Thus, if a
C (constrained) Delaunay triangulation is input, a (con-
C strained) Delaunay triangulation will result.  All indexes
C are incremented as necessary for an insertion.
C
C
C On input:
C
C       K = Nodal index (index for X, Y, and LEND) of the
C           new node to be added.  1 .LE. K .LE. LCC(1).
C           (K .LE. N+1 if NCC=0).
C
C       XK,YK = Cartesian coordinates of the new node (to be
C               stored in X(K) and Y(K)).  The node must not
C               lie in a constraint region.
C
C       IST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to node K.  1 .LE. IST .LE. N.
C
C       NCC = Number of constraint curves.  NCC .GE. 0.
C
C The above parameters are not altered by this routine.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             subroutine ADDCST.
C
C       N = Number of nodes in the triangulation before K is
C           added.  N .GE. 3.  Note that N will be incre-
C           mented following the addition of node K.
C
C       X,Y = Arrays of length at least N+1 containing the
C             Cartesian coordinates of the nodes in the
C             first N positions with non-constraint nodes
C             in the first LCC(1)-1 locations if NCC > 0.
C
C       LIST,LPTR,LEND,LNEW = Data structure associated with
C                             the triangulation of nodes 1
C                             to N.  The arrays must have
C                             sufficient length for N+1
C                             nodes.  Refer to TRMESH.
C
C On output:
C
C       LCC = List of constraint curve starting indexes in-
C             cremented by 1 to reflect the insertion of K
C             unless NCC = 0 or IER .NE. 0.
C
C       N = Number of nodes in the triangulation including K
C           unless IER .NE. 0.  Note that all comments refer
C           to the input value of N.
C
C       X,Y = Arrays updated with the insertion of XK and YK
C             in the K-th positions (node I+1 was node I be-
C             fore the insertion for I = K to N if K .LE. N)
C             unless IER .NE. 0.
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node K unless
C                             IER .NE. 0.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if K, IST, NCC, N, or an LCC entry is
C                      outside its valid range on input.
C             IER = -2 if all nodes (including K) are col-
C                      linear.
C             IER =  L if nodes L and K coincide for some L.
C             IER = -3 if K lies in a constraint region.
C
C             The errors conditions are tested in the order
C             specified.
C
C Modules required by ADDNOD:  BDYADD, CRTRI, INDXCC,
C                                INSERT, INTADD, LEFT,
C                                LSTPTR, SWAP, SWPTST,
C                                TRFIND
C
C Intrinsic function called by ADDNOD:  ABS
C
C***********************************************************
C
      INTEGER INDXCC, LSTPTR
      INTEGER I, I1, I2, I3, IBK, IO1, IO2, IN1, KK, L,
     .        LCCIP1, LP, LPF, LPO1, NM1
      LOGICAL CRTRI, SWPTST
      KK = K
C
C Test for an invalid input parameter.
C
      IF (KK .LT. 1  .OR.  IST .LT. 1  .OR.  IST .GT. N
     .    .OR.  NCC .LT. 0  .OR.  N .LT. 3) GO TO 7
      LCCIP1 = N+1
      DO 1 I = NCC,1,-1
        IF (LCCIP1-LCC(I) .LT. 3) GO TO 7
        LCCIP1 = LCC(I)
    1   CONTINUE
      IF (KK .GT. LCCIP1) GO TO 7
C
C Find a triangle (I1,I2,I3) containing K or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from node K.
C
      CALL TRFIND (IST,XK,YK,X,Y,LIST,LPTR,LEND, I1,I2,I3)
C
C Test for collinear nodes, duplicate nodes, and K lying in
C   a constraint region.
C
      IF (I1 .EQ. 0) GO TO 8
      IF (I3 .NE. 0) THEN
        L = I1
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        L = I2
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        L = I3
        IF (XK .EQ. X(L)  .AND.  YK .EQ. Y(L)) GO TO 9
        IF (NCC .GT. 0  .AND.  CRTRI(NCC,LCC,I1,I2,I3) )
     .    GO TO 10
      ELSE
C
C   K is outside the convex hull of the nodes and lies in a
C     constraint region iff an exterior constraint curve is
C     present.
C
        IF (NCC .GT. 0  .AND.  INDXCC(NCC,LCC,N,LIST,LEND)
     .      .NE. 0) GO TO 10
      ENDIF
C
C No errors encountered.
C
      IER = 0
      NM1 = N
      N = N + 1
      IF (KK .LT. N) THEN
C
C Open a slot for K in X, Y, and LEND, and increment all
C   nodal indexes which are greater than or equal to K.
C   Note that LIST, LPTR, and LNEW are not yet updated with
C   either the neighbors of K or the edges terminating on K.
C
        DO 2 IBK = NM1,KK,-1
          X(IBK+1) = X(IBK)
          Y(IBK+1) = Y(IBK)
          LEND(IBK+1) = LEND(IBK)
    2     CONTINUE
        DO 3 I = 1,NCC
          LCC(I) = LCC(I) + 1
    3     CONTINUE
        L = LNEW - 1
        DO 4 I = 1,L
          IF (LIST(I) .GE. KK) LIST(I) = LIST(I) + 1
          IF (LIST(I) .LE. -KK) LIST(I) = LIST(I) - 1
    4     CONTINUE
        IF (I1 .GE. KK) I1 = I1 + 1
        IF (I2 .GE. KK) I2 = I2 + 1
        IF (I3 .GE. KK) I3 = I3 + 1
      ENDIF
C
C Insert K into X and Y, and update LIST, LPTR, LEND, and
C   LNEW with the arcs containing node K.
C
      X(KK) = XK
      Y(KK) = YK
      IF (I3 .EQ. 0) THEN
        CALL BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
      ELSE
        CALL INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      ENDIF
C
C Initialize variables for optimization of the triangula-
C   tion.
C
      LP = LEND(KK)
      LPF = LPTR(LP)
      IO2 = LIST(LPF)
      LPO1 = LPTR(LPF)
      IO1 = ABS(LIST(LPO1))
C
C Begin loop:  find the node opposite K.
C
    5 LP = LSTPTR(LEND(IO1),IO2,LIST,LPTR)
      IF (LIST(LP) .LT. 0) GO TO 6
      LP = LPTR(LP)
      IN1 = ABS(LIST(LP))
      IF ( CRTRI(NCC,LCC,IO1,IO2,IN1) ) GO TO 6
C
C Swap test:  if a swap occurs, two new arcs are
C             opposite K and must be tested.
C
      IF ( .NOT. SWPTST(IN1,KK,IO1,IO2,X,Y) ) GO TO 6
      CALL SWAP (IN1,KK,IO1,IO2, LIST,LPTR,LEND, LPO1)
      IO1 = IN1
      GO TO 5
C
C No swap occurred.  Test for termination and reset
C   IO2 and IO1.
C
    6 IF (LPO1 .EQ. LPF  .OR.  LIST(LPO1) .LT. 0) RETURN
      IO2 = IO1
      LPO1 = LPTR(LPO1)
      IO1 = ABS(LIST(LPO1))
      GO TO 5
C
C A parameter is outside its valid range on input.
C
    7 IER = -1
      RETURN
C
C All nodes are collinear.
C
    8 IER = -2
      RETURN
C
C Nodes L and K coincide.
C
    9 IER = L
      RETURN
C
C Node K lies in a constraint region.
C
   10 IER = -3
      RETURN
      END
      SUBROUTINE BDYADD (KK,I1,I2, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, LIST(*), LPTR(*), LEND(*), LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   02/22/91
C
C   This subroutine adds a boundary node to a triangulation
C of a set of points in the plane.  The data structure is
C updated with the insertion of node KK, but no optimization
C is performed.
C
C
C On input:
C
C       KK = Index of a node to be connected to the sequence
C            of all visible boundary nodes.  KK .GE. 1 and
C            KK must not be equal to I1 or I2.
C
C       I1 = First (rightmost as viewed from KK) boundary
C            node in the triangulation which is visible from
C            node KK (the line segment KK-I1 intersects no
C            arcs.
C
C       I2 = Last (leftmost) boundary node which is visible
C            from node KK.  I1 and I2 may be determined by
C            subroutine TRFIND.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Triangulation data structure
C                             created by TRMESH or TRMSHR.
C                             Nodes I1 and I2 must be in-
C                             cluded in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  Node
C                             KK is connected to I1, I2, and
C                             all boundary nodes in between.
C
C Module required by BDYADD:  INSERT
C
C***********************************************************
C
      INTEGER K, LP, LSAV, N1, N2, NEXT, NSAV
      K = KK
      N1 = I1
      N2 = I2
C
C Add K as the last neighbor of N1.
C
      LP = LEND(N1)
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = -K
      LPTR(LNEW) = LSAV
      LEND(N1) = LNEW
      LNEW = LNEW + 1
      NEXT = -LIST(LP)
      LIST(LP) = NEXT
      NSAV = NEXT
C
C Loop on the remaining boundary nodes between N1 and N2,
C   adding K as the first neighbor.
C
    1 LP = LEND(NEXT)
        CALL INSERT (K,LP,LIST,LPTR,LNEW)
        IF (NEXT .EQ. N2) GO TO 2
        NEXT = -LIST(LP)
        LIST(LP) = NEXT
        GO TO 1
C
C Add the boundary nodes between N1 and N2 as neighbors
C   of node K.
C
    2 LSAV = LNEW
      LIST(LNEW) = N1
      LPTR(LNEW) = LNEW + 1
      LNEW = LNEW + 1
      NEXT = NSAV
C
    3 IF (NEXT .EQ. N2) GO TO 4
        LIST(LNEW) = NEXT
        LPTR(LNEW) = LNEW + 1
        LNEW = LNEW + 1
        LP = LEND(NEXT)
        NEXT = LIST(LP)
        GO TO 3
C
    4 LIST(LNEW) = -N2
      LPTR(LNEW) = LSAV
      LEND(K) = LNEW
      LNEW = LNEW + 1
      RETURN
      END
      LOGICAL FUNCTION CRTRI (NCC,LCC,I1,I2,I3)
      INTEGER NCC, LCC(*), I1, I2, I3
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/14/91
C
C   This function returns TRUE if and only if triangle (I1,
C I2,I3) lies in a constraint region.
C
C
C On input:
C
C       NCC,LCC = Constraint data structure.  Refer to sub-
C                 routine ADDCST.
C
C       I1,I2,I3 = Nodal indexes of the counterclockwise-
C                  ordered vertices of a triangle.
C
C Input parameters are altered by this function.
C
C       CRTRI = TRUE iff (I1,I2,I3) is a constraint region
C               triangle.
C
C Note that input parameters are not tested for validity.
C
C Modules required by CRTRI:  None
C
C Intrinsic functions called by CRTRI:  MAX, MIN
C
C***********************************************************
C
      INTEGER I, IMAX, IMIN
      IMAX = MAX(I1,I2,I3)
C
C   Find the index I of the constraint containing IMAX.
C
      I = NCC + 1
    1 I = I - 1
        IF (I .LE. 0) GO TO 2
        IF (IMAX .LT. LCC(I)) GO TO 1
      IMIN = MIN(I1,I2,I3)
C
C P lies in a constraint region iff I1, I2, and I3 are nodes
C   of the same constraint (IMIN >= LCC(I)), and (IMIN,IMAX)
C   is (I1,I3), (I2,I1), or (I3,I2).
C
      CRTRI = IMIN .GE. LCC(I)  .AND.  ((IMIN .EQ. I1 .AND.
     .        IMAX .EQ. I3)  .OR.  (IMIN .EQ. I2  .AND.
     .        IMAX .EQ. I1)  .OR.  (IMIN .EQ. I3  .AND.
     .        IMAX .EQ. I2))
      RETURN
C
C NCC .LE. 0 or all vertices are non-constraint nodes.
C
    2 CRTRI = .FALSE.
      RETURN
      END
      INTEGER FUNCTION INDXCC (NCC,LCC,N,LIST,LEND)
      INTEGER NCC, LCC(*), N, LIST(*), LEND(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/25/91
C
C   Given a constrained Delaunay triangulation, this func-
C tion returns the index, if any, of an exterior constraint
C curve (an unbounded constraint region).  An exterior con-
C straint curve is assumed to be present if and only if the
C clockwise-ordered sequence of boundary nodes is a subse-
C quence of a constraint node sequence.  The triangulation
C adjacencies corresponding to constraint edges may or may
C not have been forced by a call to ADDCST, and the con-
C straint region may or may not be valid (contain no nodes).
C
C
C On input:
C
C       NCC = Number of constraints.  NCC .GE. 0.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             subroutine ADDCST.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LEND = Data structure defining the triangula-
C                   tion.  Refer to subroutine TRMESH.
C
C   Input parameters are not altered by this function.  Note
C that the parameters are not tested for validity.
C
C On output:
C
C       INDXCC = Index of the exterior constraint curve, if
C                present, or 0 otherwise.
C
C Modules required by INDXCC:  None
C
C***********************************************************
C
      INTEGER I, IFRST, ILAST, LP, N0, NST, NXT
      INDXCC = 0
      IF (NCC .LT. 1) RETURN
C
C Set N0 to the boundary node with smallest index.
C
      N0 = 0
    1 N0 = N0 + 1
        LP = LEND(N0)
        IF (LIST(LP) .GT. 0) GO TO 1
C
C Search in reverse order for the constraint I, if any, that
C   contains N0.  IFRST and ILAST index the first and last
C   nodes in constraint I.
C
      I = NCC
      ILAST = N
    2 IFRST = LCC(I)
        IF (N0 .GE. IFRST) GO TO 3
        IF (I .EQ. 1) RETURN
        I = I - 1
        ILAST = IFRST - 1
        GO TO 2
C
C N0 is in constraint I which indexes an exterior constraint
C   curve iff the clockwise-ordered sequence of boundary
C   node indexes beginning with N0 is increasing and bounded
C   above by ILAST.
C
    3 NST = N0
C
    4 NXT = -LIST(LP)
        IF (NXT .EQ. NST) GO TO 5
        IF (NXT .LE. N0  .OR.  NXT .GT. ILAST) RETURN
        N0 = NXT
        LP = LEND(N0)
        GO TO 4
C
C Constraint I contains the boundary node sequence as a
C   subset.
C
    5 INDXCC = I
      RETURN
      END
      SUBROUTINE INSERT (K,LP, LIST,LPTR,LNEW )
      INTEGER K, LP, LIST(*), LPTR(*), LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   This subroutine inserts K as a neighbor of N1 following
C N2, where LP is the LIST pointer of N2 as a neighbor of
C N1.  Note that, if N2 is the last neighbor of N1, K will
C become the first neighbor (even if N1 is a boundary node).
C
C
C On input:
C
C       K = Index of the node to be inserted.
C
C       LP = LIST pointer of N2 as a neighbor of N1.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LNEW = Data structure defining the trian-
C                        gulation.  Refer to subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LNEW = Data structure updated with the
C                        addition of node K.
C
C Modules required by INSERT:  None
C
C***********************************************************
C
      INTEGER LSAV
C
      LSAV = LPTR(LP)
      LPTR(LP) = LNEW
      LIST(LNEW) = K
      LPTR(LNEW) = LSAV
      LNEW = LNEW + 1
      RETURN
      END
      SUBROUTINE INTADD (KK,I1,I2,I3, LIST,LPTR,LEND,LNEW )
      INTEGER KK, I1, I2, I3, LIST(*), LPTR(*), LEND(*),
     .        LNEW
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   02/22/91
C
C   This subroutine adds an interior node to a triangulation
C of a set of points in the plane.  The data structure is
C updated with the insertion of node KK into the triangle
C whose vertices are I1, I2, and I3.  No optimization of the
C triangulation is performed.
C
C
C On input:
C
C       KK = Index of the node to be inserted.  KK .GE. 1
C            and KK must not be equal to I1, I2, or I3.
C
C       I1,I2,I3 = Indexes of the counterclockwise-ordered
C                  sequence of vertices of a triangle which
C                  contains node KK.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to sub-
C                             routine TRMESH.  Triangle
C                             (I1,I2,I3) must be included
C                             in the triangulation.
C
C On output:
C
C       LIST,LPTR,LEND,LNEW = Data structure updated with
C                             the addition of node KK.  KK
C                             will be connected to nodes I1,
C                             I2, and I3.
C
C Modules required by INTADD:  INSERT, LSTPTR
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER K, LP, N1, N2, N3
      K = KK
C
C Initialization.
C
      N1 = I1
      N2 = I2
      N3 = I3
C
C Add K as a neighbor of I1, I2, and I3.
C
      LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
      LP = LSTPTR(LEND(N2),N3,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
      LP = LSTPTR(LEND(N3),N1,LIST,LPTR)
      CALL INSERT (K,LP,LIST,LPTR,LNEW)
C
C Add I1, I2, and I3 as neighbors of K.
C
      LIST(LNEW) = N1
      LIST(LNEW+1) = N2
      LIST(LNEW+2) = N3
      LPTR(LNEW) = LNEW + 1
      LPTR(LNEW+1) = LNEW + 2
      LPTR(LNEW+2) = LNEW
      LEND(K) = LNEW + 2
      LNEW = LNEW + 3
      RETURN
      END
      LOGICAL FUNCTION INTSEC (X1,Y1,X2,Y2,X3,Y3,X4,Y4)
      DOUBLE PRECISION X1, Y1, X2, Y2, X3, Y3, X4, Y4
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   Given a pair of line segments P1-P2 and P3-P4, this
C function returns the value .TRUE. if and only if P1-P2
C shares one or more points with P3-P4.  The line segments
C include their endpoints, and the four points need not be
C distinct.  Thus, either line segment may consist of a
C single point, and the segments may meet in a V (which is
C treated as an intersection).  Note that an incorrect
C decision may result from floating point error if the four
C endpoints are nearly collinear.
C
C
C On input:
C
C       X1,Y1 = Coordinates of P1.
C
C       X2,Y2 = Coordinates of P2.
C
C       X3,Y3 = Coordinates of P3.
C
C       X4,Y4 = Coordinates of P4.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       INTSEC = Logical value defined above.
C
C Modules required by INTSEC:  None
C
C***********************************************************
C
      DOUBLE PRECISION A, B, D, DX12, DX31, DX34, DY12, DY31, DY34
C
C Test for overlap between the smallest rectangles that
C   contain the line segments and have sides parallel to
C   the axes.
C
      IF ((X1 .LT. X3  .AND.  X1 .LT. X4  .AND.  X2 .LT. X3
     .     .AND.  X2 .LT. X4)  .OR.
     .    (X1 .GT. X3  .AND.  X1 .GT. X4  .AND.  X2 .GT. X3
     .     .AND.  X2 .GT. X4)  .OR.
     .    (Y1 .LT. Y3  .AND.  Y1 .LT. Y4  .AND.  Y2 .LT. Y3
     .     .AND.  Y2 .LT. Y4)  .OR.
     .    (Y1 .GT. Y3  .AND.  Y1 .GT. Y4  .AND.  Y2 .GT. Y3
     .     .AND.  Y2 .GT. Y4)) THEN
        INTSEC = .FALSE.
        RETURN
      ENDIF
C
C Compute A = P4-P3 X P1-P3, B = P2-P1 X P1-P3, and
C   D = P2-P1 X P4-P3 (Z components).
C
      DX12 = X2 - X1
      DY12 = Y2 - Y1
      DX34 = X4 - X3
      DY34 = Y4 - Y3
      DX31 = X1 - X3
      DY31 = Y1 - Y3
      A = DX34*DY31 - DX31*DY34
      B = DX12*DY31 - DX31*DY12
      D = DX12*DY34 - DX34*DY12
      IF (D .EQ. 0.) GO TO 1
C
C D .NE. 0 and the point of intersection of the lines de-
C   fined by the line segments is P = P1 + (A/D)*(P2-P1) =
C   P3 + (B/D)*(P4-P3).
C
      INTSEC = A/D .GE. 0.  .AND.  A/D .LE. 1.  .AND.
     .         B/D .GE. 0.  .AND.  B/D .LE. 1.
      RETURN
C
C D .EQ. 0 and thus either the line segments are parallel,
C   or one (or both) of them is a single point.
C
    1 INTSEC = A .EQ. 0.  .AND.  B .EQ. 0.
      RETURN
      END
      LOGICAL FUNCTION LEFT (X1,Y1,X2,Y2,X0,Y0)
      DOUBLE PRECISION    X1, Y1, X2, Y2, X0, Y0
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   This function determines whether node N0 is to the left
C or to the right of the line through N1-N2 as viewed by an
C observer at N1 facing N2.
C
C
C On input:
C
C       X1,Y1 = Coordinates of N1.
C
C       X2,Y2 = Coordinates of N2.
C
C       X0,Y0 = Coordinates of N0.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LEFT = .TRUE. if and only if (X0,Y0) is on or to the
C              left of the directed line N1->N2.
C
C Modules required by LEFT:  None
C
C***********************************************************
C
      DOUBLE PRECISION DX1, DY1, DX2, DY2
C
C Local parameters:
C
C DX1,DY1 = X,Y components of the vector N1->N2
C DX2,DY2 = X,Y components of the vector N1->N0
C
      DX1 = X2-X1
      DY1 = Y2-Y1
      DX2 = X0-X1
      DY2 = Y0-Y1
C
C If the sign of the vector cross product of N1->N2 and
C   N1->N0 is positive, then sin(A) > 0, where A is the
C   angle between the vectors, and thus A is in the range
C   (0,180) degrees.
C
      LEFT = DX1*DY2 .GE. DX2*DY1
      RETURN
      END
      INTEGER FUNCTION LSTPTR (LPL,NB,LIST,LPTR)
      INTEGER LPL, NB, LIST(*), LPTR(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   This function returns the index (LIST pointer) of NB in
C the adjacency list for N0, where LPL = LEND(N0).
C
C
C On input:
C
C       LPL = LEND(N0)
C
C       NB = Index of the node whose pointer is to be re-
C            turned.  NB must be connected to N0.
C
C       LIST,LPTR = Data structure defining the triangula-
C                   tion.  Refer to subroutine TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       LSTPTR = Pointer such that LIST(LSTPTR) = NB or
C                LIST(LSTPTR) = -NB, unless NB is not a
C                neighbor of N0, in which case LSTPTR = LPL.
C
C Modules required by LSTPTR:  None
C
C***********************************************************
C
      INTEGER LP, ND
C
      LP = LPTR(LPL)
    1 ND = LIST(LP)
        IF (ND .EQ. NB) GO TO 2
        LP = LPTR(LP)
        IF (LP .NE. LPL) GO TO 1
C
    2 LSTPTR = LP
      RETURN
      END
      INTEGER FUNCTION NBCNT (LPL,LPTR)
      INTEGER LPL, LPTR(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   This function returns the number of neighbors of a node
C N0 in a triangulation created by subroutine TRMESH (or
C TRMSHR).
C
C
C On input:
C
C       LPL = LIST pointer to the last neighbor of N0 --
C             LPL = LEND(N0).
C
C       LPTR = Array of pointers associated with LIST.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NBCNT = Number of neighbors of N0.
C
C Modules required by NBCNT:  None
C
C***********************************************************
C
      INTEGER K, LP
C
      LP = LPL
      K = 1
C
    1 LP = LPTR(LP)
        IF (LP .EQ. LPL) GO TO 2
        K = K + 1
        GO TO 1
C
    2 NBCNT = K
      RETURN
      END
      INTEGER FUNCTION NEARND (XP,YP,IST,N,X,Y,LIST,LPTR,
     .                         LEND, DSQ)
      INTEGER IST, N, LIST(*), LPTR(*), LEND(N)
      DOUBLE PRECISION    XP, YP, X(N), Y(N), DSQ
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   10/31/90
C
C   Given a point P in the plane and a Delaunay triangula-
C tion created by subroutine TRMESH or TRMSHR, this function
C returns the index of the nearest triangulation node to P.
C
C   The algorithm consists of implicitly adding P to the
C triangulation, finding the nearest neighbor to P, and
C implicitly deleting P from the triangulation.  Thus, it
C is based on the fact that, if P is a node in a Delaunay
C triangulation, the nearest node to P is a neighbor of P.
C
C
C On input:
C
C       XP,YP = Cartesian coordinates of the point P to be
C               located relative to the triangulation.
C
C       IST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of the nodes.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to TRMESH.
C
C Input parameters are not altered by this function.
C
C On output:
C
C       NEARND = Nodal index of the nearest node to P, or 0
C                if N < 3 or the triangulation data struc-
C                ture is invalid.
C
C       DSQ = Squared distance between P and NEARND unless
C             NEARND = 0.
C
C       Note that the number of candidates for NEARND
C       (neighbors of P) is limited to LMAX defined in
C       the PARAMETER statement below.
C
C Modules required by NEARND:  LEFT, LSTPTR, TRFIND
C
C Intrinsic function called by NEARND:  ABS
C
C***********************************************************
C
      INTEGER   LSTPTR
      INTEGER   LMAX
      PARAMETER (LMAX=25)
      INTEGER   I1, I2, I3, L, LISTP(LMAX), LP, LP1, LP2,
     .          LPL, LPTRP(LMAX), N1, N2, N3, NN, NR, NST
      DOUBLE PRECISION      COS1, COS2, DS1, DSR, DX11, DX12, DX21,
     .          DX22, DY11, DY12, DY21, DY22, SIN1, SIN2
C
C Store local parameters and test for N invalid.
C
      NN = N
      IF (NN .LT. 3) GO TO 7
      NST = IST
      IF (NST .LT. 1  .OR.  NST .GT. NN) NST = 1
C
C Find a triangle (I1,I2,I3) containing P, or the rightmost
C   (I1) and leftmost (I2) visible boundary nodes as viewed
C   from P.
C
      CALL TRFIND (NST,XP,YP,X,Y,LIST,LPTR,LEND, I1,I2,I3)
C
C Test for collinear nodes.
C
      IF (I1 .EQ. 0) GO TO 7
C
C Store the linked list of 'neighbors' of P in LISTP and
C   LPTRP.  I1 is the first neighbor, and 0 is stored as
C   the last neighbor if P is not contained in a triangle.
C   L is the length of LISTP and LPTRP, and is limited to
C   LMAX.
C
      IF (I3 .NE. 0) THEN
        LISTP(1) = I1
        LPTRP(1) = 2
        LISTP(2) = I2
        LPTRP(2) = 3
        LISTP(3) = I3
        LPTRP(3) = 1
        L = 3
      ELSE
        N1 = I1
        L = 1
        LP1 = 2
        LISTP(L) = N1
        LPTRP(L) = LP1
C
C   Loop on the ordered sequence of visible boundary nodes
C     N1 from I1 to I2.
C
    1   LPL = LEND(N1)
          N1 = -LIST(LPL)
          L = LP1
          LP1 = L+1
          LISTP(L) = N1
          LPTRP(L) = LP1
          IF (N1 .NE. I2  .AND.  LP1 .LT. LMAX) GO TO 1
        L = LP1
        LISTP(L) = 0
        LPTRP(L) = 1
      ENDIF
C
C Initialize variables for a loop on arcs N1-N2 opposite P
C   in which new 'neighbors' are 'swapped' in.  N1 follows
C   N2 as a neighbor of P, and LP1 and LP2 are the LISTP
C   indexes of N1 and N2.
C
      LP2 = 1
      N2 = I1
      LP1 = LPTRP(1)
      N1 = LISTP(LP1)
C
C Begin loop:  find the node N3 opposite N1->N2.
C
    2 LP = LSTPTR(LEND(N1),N2,LIST,LPTR)
        IF (LIST(LP) .LT. 0) GO TO 4
        LP = LPTR(LP)
        N3 = ABS(LIST(LP))
C
C Swap test:  Exit the loop if L = LMAX.
C
        IF (L .EQ. LMAX) GO TO 5
        DX11 = X(N1) - X(N3)
        DX12 = X(N2) - X(N3)
        DX22 = X(N2) - XP
        DX21 = X(N1) - XP
C
        DY11 = Y(N1) - Y(N3)
        DY12 = Y(N2) - Y(N3)
        DY22 = Y(N2) - YP
        DY21 = Y(N1) - YP
C
        COS1 = DX11*DX12 + DY11*DY12
        COS2 = DX22*DX21 + DY22*DY21
        IF (COS1 .GE. 0.  .AND.  COS2 .GE. 0.) GO TO 4
        IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 3
C
        SIN1 = DX11*DY12 - DX12*DY11
        SIN2 = DX22*DY21 - DX21*DY22
        IF (SIN1*COS2 + COS1*SIN2 .GE. 0.) GO TO 4
C
C Swap:  Insert N3 following N2 in the adjacency list for P.
C        The two new arcs opposite P must be tested.
C
    3   L = L+1
        LPTRP(LP2) = L
        LISTP(L) = N3
        LPTRP(L) = LP1
        LP1 = L
        N1 = N3
        GO TO 2
C
C No swap:  Advance to the next arc and test for termination
C           on N1 = I1 (LP1 = 1) or N1 followed by 0.
C
    4   IF (LP1 .EQ. 1) GO TO 5
        LP2 = LP1
        N2 = N1
        LP1 = LPTRP(LP1)
        N1 = LISTP(LP1)
        IF (N1 .EQ. 0) GO TO 5
        GO TO 2
C
C Set NR and DSR to the index of the nearest node to P and
C   its squared distance from P, respectively.
C
    5 NR = I1
      DSR = (X(NR)-XP)**2 + (Y(NR)-YP)**2
      DO 6 LP = 2,L
        N1 = LISTP(LP)
        IF (N1 .EQ. 0) GO TO 6
        DS1 = (X(N1)-XP)**2 + (Y(N1)-YP)**2
        IF (DS1 .LT. DSR) THEN
          NR = N1
          DSR = DS1
        ENDIF
    6   CONTINUE
      DSQ = DSR
      NEARND = NR
      RETURN
C
C Invalid input.
C
    7 NEARND = 0
      RETURN
      END
      DOUBLE PRECISION FUNCTION STORE (X)
      DOUBLE PRECISION X
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   03/18/90
C
C   This function forces its argument X to be stored in a
C memory location, thus providing a means of determining
C floating point number characteristics (such as the machine
C precision) when it is necessary to avoid computation in
C high precision registers.
C
C
C On input:
C
C       X = Value to be stored.
C
C X is not altered by this function.
C
C On output:
C
C       STORE = Value of X after it has been stored and
C               possibly truncated or rounded to the single
C               precision word length.
C
C Modules required by STORE:  None
C
C***********************************************************
C
      DOUBLE PRECISION Y
      COMMON/STCOM/Y
C
      Y = X
      STORE = Y
      RETURN
      END
      SUBROUTINE SWAP (IN1,IN2,IO1,IO2, LIST,LPTR,
     .                 LEND, LP21)
      INTEGER IN1, IN2, IO1, IO2, LIST(*), LPTR(*), LEND(*),
     .        LP21
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   Given a triangulation of a set of points in the plane,
C this subroutine replaces a diagonal arc in a strictly
C convex quadrilateral (defined by a pair of adjacent tri-
C angles) with the other diagonal.
C
C
C On input:
C
C       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of
C                         the quadrilateral.  IO1-IO2 is re-
C                         placed by IN1-IN2.  (IO1,IO2,IN1)
C                         and (IO2,IO1,IN2) must be trian-
C                         gles on input.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to subroutine
C                        TRMESH.
C
C On output:
C
C       LIST,LPTR,LEND = Data structure updated with the
C                        swap -- triangles (IO1,IO2,IN1) and
C                        (IO2,IO1,IN2) are replaced by
C                        (IN1,IN2,IO2) and (IN2,IN1,IO1).
C
C       LP21 = Index of IN1 as a neighbor of IN2 after the
C              swap is performed.
C
C Module required by SWAP:  LSTPTR
C
C***********************************************************
C
      INTEGER LSTPTR
      INTEGER LP, LPH, LPSAV
C
C Delete IO2 as a neighbor of IO1.
C
      LP = LSTPTR(LEND(IO1),IN2,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO2 is the last neighbor of IO1, make IN2 the
C   last neighbor.
C
      IF (LEND(IO1) .EQ. LPH) LEND(IO1) = LP
C
C Insert IN2 as a neighbor of IN1 following IO1
C   using the hole created above.
C
      LP = LSTPTR(LEND(IN1),IO1,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN2
      LPTR(LPH) = LPSAV
C
C Delete IO1 as a neighbor of IO2.
C
      LP = LSTPTR(LEND(IO2),IN1,LIST,LPTR)
      LPH = LPTR(LP)
      LPTR(LP) = LPTR(LPH)
C
C If IO1 is the last neighbor of IO2, make IN1 the
C   last neighbor.
C
      IF (LEND(IO2) .EQ. LPH) LEND(IO2) = LP
C
C Insert IN1 as a neighbor of IN2 following IO2.
C
      LP = LSTPTR(LEND(IN2),IO2,LIST,LPTR)
      LPSAV = LPTR(LP)
      LPTR(LP) = LPH
      LIST(LPH) = IN1
      LPTR(LPH) = LPSAV
      LP21 = LPH
      RETURN
      END
      LOGICAL FUNCTION SWPTST (IN1,IN2,IO1,IO2,X,Y)
      INTEGER IN1, IN2, IO1, IO2
      DOUBLE PRECISION    X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   09/01/88
C
C   This function applies the circumcircle test to a quadri-
C lateral defined by a pair of adjacent triangles.  The
C diagonal arc (shared triangle side) should be swapped for
C the other diagonl if and only if the fourth vertex is
C strictly interior to the circumcircle of one of the
C triangles (the decision is independent of the choice of
C triangle).  Equivalently, the diagonal is chosen to maxi-
C mize the smallest of the six interior angles over the two
C pairs of possible triangles (the decision is for no swap
C if the quadrilateral is not strictly convex).
C
C   When the four vertices are nearly cocircular (the
C neutral case), the preferred decision is no swap -- in
C order to avoid unnecessary swaps and, more important, to
C avoid cycling in subroutine OPTIM which is called by
C DELNOD and EDGE.  Thus, a tolerance SWTOL (stored in
C SWPCOM by TRMESH or TRMSHR) is used to define 'nearness'
C to the neutral case.
C
C
C On input:
C
C       IN1,IN2,IO1,IO2 = Nodal indexes of the vertices of
C                         the quadrilateral.  IO1-IO2 is the
C                         triangulation arc (shared triangle
C                         side) to be replaced by IN1-IN2 if
C                         the decision is to swap.  The
C                         triples (IO1,IO2,IN1) and (IO2,
C                         IO1,IN2) must define triangles (be
C                         in counterclockwise order) on in-
C                         put.
C
C       X,Y = Arrays containing the nodal coordinates.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       SWPTST = .TRUE. if and only if the arc connecting
C                IO1 and IO2 is to be replaced.
C
C Modules required by SWPTST:  None
C
C***********************************************************
C
      DOUBLE PRECISION DX11, DX12, DX22, DX21, DY11, DY12, DY22, DY21,
     .     SIN1, SIN2, COS1, COS2, SIN12, SWTOL
C
C Tolerance stored by TRMESH or TRMSHR.
C
      COMMON/SWPCOM/SWTOL
C
C Local parameters:
C
C DX11,DY11 = X,Y components of the vector IN1->IO1
C DX12,DY12 = X,Y components of the vector IN1->IO2
C DX22,DY22 = X,Y components of the vector IN2->IO2
C DX21,DY21 = X,Y components of the vector IN2->IO1
C SIN1 =      Cross product of the vectors IN1->IO1 and
C               IN1->IO2 -- proportional to sin(T1), where
C               T1 is the angle at IN1 formed by the vectors
C COS1 =      Inner product of the vectors IN1->IO1 and
C               IN1->IO2 -- proportional to cos(T1)
C SIN2 =      Cross product of the vectors IN2->IO2 and
C               IN2->IO1 -- proportional to sin(T2), where
C               T2 is the angle at IN2 formed by the vectors
C COS2 =      Inner product of the vectors IN2->IO2 and
C               IN2->IO1 -- proportional to cos(T2)
C SIN12 =     SIN1*COS2 + COS1*SIN2 -- proportional to
C               sin(T1+T2)
C
C
C Compute the vectors containing the angles T1 and T2.
C
      DX11 = X(IO1) - X(IN1)
      DX12 = X(IO2) - X(IN1)
      DX22 = X(IO2) - X(IN2)
      DX21 = X(IO1) - X(IN2)
C
      DY11 = Y(IO1) - Y(IN1)
      DY12 = Y(IO2) - Y(IN1)
      DY22 = Y(IO2) - Y(IN2)
      DY21 = Y(IO1) - Y(IN2)
C
C Compute inner products.
C
      COS1 = DX11*DX12 + DY11*DY12
      COS2 = DX22*DX21 + DY22*DY21
C
C The diagonals should be swapped iff (T1+T2) > 180
C   degrees.  The following two tests ensure numerical
C   stability:  the decision must be FALSE when both
C   angles are close to 0, and TRUE when both angles
C   are close to 180 degrees.
C
      IF (COS1 .GE. 0.  .AND.  COS2 .GE. 0.) GO TO 2
      IF (COS1 .LT. 0.  .AND.  COS2 .LT. 0.) GO TO 1
C
C Compute vector cross products (Z-components).
C
      SIN1 = DX11*DY12 - DX12*DY11
      SIN2 = DX22*DY21 - DX21*DY22
      SIN12 = SIN1*COS2 + COS1*SIN2
      IF (SIN12 .GE. -SWTOL) GO TO 2
C
C Swap.
C
    1 SWPTST = .TRUE.
      RETURN
C
C No swap.
C
    2 SWPTST = .FALSE.
      RETURN
      END
      SUBROUTINE TRFIND (NST,PX,PY,X,Y,LIST,LPTR,LEND, I1,
     .                   I2,I3)
      INTEGER NST, LIST(*), LPTR(*), LEND(*), I1, I2, I3
      DOUBLE PRECISION    PX, PY, X(*), Y(*)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   06/14/90
C
C   This subroutine locates a point P relative to a triangu-
C lation created by subroutine TRMESH or TRMSHR.  If P is
C contained in a triangle, the three vertex indexes are
C returned.  Otherwise, the indexes of the rightmost and
C leftmost visible boundary nodes are returned.
C
C
C On input:
C
C       NST = Index of a node at which TRFIND begins the
C             search.  Search time depends on the proximity
C             of this node to P.
C
C       PX,PY = X and Y coordinates of the point P to be
C               located.
C
C       X,Y = Arrays containing the coordinates of the nodes
C             in the triangulation.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to subroutine
C                        TRMESH.
C
C Input parameters are not altered by this routine.
C
C On output:
C
C       I1,I2,I3 = Nodal indexes, in counterclockwise order,
C                  of the vertices of a triangle containing
C                  P, or, if P is not contained in the con-
C                  vex hull of the nodes, I1 indexes the
C                  rightmost visible boundary node, I2 in-
C                  dexes the leftmost visible boundary node,
C                  and I3 = 0.  Rightmost and leftmost are
C                  defined from the perspective of P, and a
C                  pair of points are visible from each
C                  other if and only if the line segment
C                  joining them intersects no triangulation
C                  arc.  If P and all of the nodes lie on a
C                  common line, then I1 = I2 = I3 = 0 on
C                  output.
C
C Modules required by TRFIND:  LEFT, LSTPTR
C
C Intrinsic functions called by TRFIND:  ABS, MAX
C
C***********************************************************
C
      INTEGER LSTPTR
      LOGICAL LEFT
      INTEGER LP, N0, N1, N2, N3, N4, NB, NF, NL, NP, NPP
      LOGICAL FRWRD
      DOUBLE PRECISION    XA, XB, XC, XP, YA, YB, YC, YP
C
C FRWRD = TRUE iff C is forward of A->B
C              iff <A->B,A->C> .GE. 0.
C
      FRWRD(XA,YA,XB,YB,XC,YC) = (XB-XA)*(XC-XA) +
     .                           (YB-YA)*(YC-YA) .GE. 0.
C
      N0 = MAX(NST,1)
      XP = PX
      YP = PY
C
C Set N1 = NF and NL to the first and last neighbors of N0.
C
    1 LP = LEND(N0)
      NL = LIST(LP)
      LP = LPTR(LP)
      NF = LIST(LP)
      N1 = NF
C
C Find a pair of adjacent neighbors N1,N2 of N0 which define
C   a wedge containing P:  P LEFT N0->N1 and P RIGHT N0->N2.
C
      IF (NL .GT. 0) GO TO 2
C
C   N0 is a boundary node.  Test for P exterior.
C
      NL = -NL
      IF ( .NOT. LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) ) THEN
        NL = N0
        GO TO 9
      ENDIF
      IF ( .NOT. LEFT(X(NL),Y(NL),X(N0),Y(N0),XP,YP) ) THEN
        NB = NF
        NF = N0
        NP = NL
        NPP = N0
        GO TO 11
      ENDIF
      GO TO 3
C
C   N0 is an interior node.  Find N1.
C
    2 IF ( LEFT(X(N0),Y(N0),X(N1),Y(N1),XP,YP) ) GO TO 3
        LP = LPTR(LP)
        N1 = LIST(LP)
        IF (N1 .EQ. NL) GO TO 6
        GO TO 2
C
C   P is to the left of edge N0->N1.  Initialize N2 to the
C     next neighbor of N0.
C
    3 LP = LPTR(LP)
        N2 = ABS(LIST(LP))
        IF ( .NOT. LEFT(X(N0),Y(N0),X(N2),Y(N2),XP,YP) )
     .    GO TO 7
        N1 = N2
        IF (N1 .NE. NL) GO TO 3
      IF ( .NOT. LEFT(X(N0),Y(N0),X(NF),Y(NF),XP,YP) )
     .  GO TO 6
      IF (XP .EQ. X(N0) .AND. YP .EQ. Y(N0)) GO TO 5
C
C   P is left of or on edges N0->NB for all neighbors NB
C     of N0.
C   All points are collinear iff P is left of NB->N0 for
C     all neighbors NB of N0.  Search the neighbors of N0.
C     NOTE -- N1 = NL and LP points to NL.
C
    4 IF ( .NOT. LEFT(X(N1),Y(N1),X(N0),Y(N0),XP,YP) )
     .  GO TO 5
        LP = LPTR(LP)
        N1 = ABS(LIST(LP))
        IF (N1 .EQ. NL) GO TO 17
        GO TO 4
C
C   P is to the right of N1->N0, or P=N0.  Set N0 to N1 and
C     start over.
C
    5 N0 = N1
      GO TO 1
C
C   P is between edges N0->N1 and N0->NF.
C
    6 N2 = NF
C
C P is contained in the wedge defined by line segments
C   N0->N1 and N0->N2, where N1 is adjacent to N2.  Set
C   N3 to the node opposite N1->N2.
C
    7 N3 = N0
C
C Top of edge hopping loop.  Test for termination.
C
    8 IF ( LEFT(X(N1),Y(N1),X(N2),Y(N2),XP,YP) ) THEN
C
C   P LEFT N1->N2 and hence P is in (N1,N2,N3) unless an
C     error resulted from floating point inaccuracy and
C     collinearity.
C
        IF ( LEFT(X(N2),Y(N2),X(N3),Y(N3),XP,YP)  .AND.
     .       LEFT(X(N3),Y(N3),X(N1),Y(N1),XP,YP) ) GO TO 16
      ENDIF
C
C   Set N4 to the neighbor of N2 which follows N1 (node
C     opposite N2->N1) unless N1->N2 is a boundary edge.
C
      LP = LSTPTR(LEND(N2),N1,LIST,LPTR)
      IF (LIST(LP) .LT. 0) THEN
        NF = N2
        NL = N1
        GO TO 9
      ENDIF
      LP = LPTR(LP)
      N4 = ABS(LIST(LP))
C
C   Select the new edge N1->N2 which intersects the line
C     segment N0-P, and set N3 to the node opposite N1->N2.
C
      IF ( LEFT(X(N0),Y(N0),X(N4),Y(N4),XP,YP) ) THEN
        N3 = N1
        N1 = N4
      ELSE
        N3 = N2
        N2 = N4
      ENDIF
      GO TO 8
C
C Boundary traversal loops.  NL->NF is a boundary edge and
C   P RIGHT NL->NF.  Save NL and NF.

    9 NP = NL
      NPP = NF
C
C Find the first (rightmost) visible boundary node NF.  NB
C   is set to the first neighbor of NF, and NP is the last
C   neighbor.
C
   10 LP = LEND(NF)
      LP = LPTR(LP)
      NB = LIST(LP)
      IF ( .NOT. LEFT(X(NF),Y(NF),X(NB),Y(NB),XP,YP) )
     .  GO TO 12
C
C   P LEFT NF->NB and thus NB is not visible unless an error
C     resulted from floating point inaccuracy and collinear-
C     ity of the 4 points NP, NF, NB, and P.
C
   11 IF ( FRWRD(X(NF),Y(NF),X(NP),Y(NP),XP,YP)  .OR.
     .     FRWRD(X(NF),Y(NF),X(NP),Y(NP),X(NB),Y(NB)) ) THEN
        I1 = NF
        GO TO 13
      ENDIF
C
C   Bottom of loop.
C
   12 NP = NF
      NF = NB
      GO TO 10
C
C Find the last (leftmost) visible boundary node NL.  NB
C   is set to the last neighbor of NL, and NPP is the first
C   neighbor.
C
   13 LP = LEND(NL)
      NB = -LIST(LP)
      IF ( .NOT. LEFT(X(NB),Y(NB),X(NL),Y(NL),XP,YP) )
     .  GO TO 14
C
C   P LEFT NB->NL and thus NB is not visible unless an error
C     resulted from floating point inaccuracy and collinear-
C     ity of the 4 points P, NB, NL, and NPP.
C
      IF ( FRWRD(X(NL),Y(NL),X(NPP),Y(NPP),XP,YP)  .OR.
     .     FRWRD(X(NL),Y(NL),X(NPP),Y(NPP),X(NB),Y(NB)) )
     .  GO TO 15
C
C   Bottom of loop.
C
   14 NPP = NL
      NL = NB
      GO TO 13
C
C NL is the leftmost visible boundary node.
C
   15 I2 = NL
      I3 = 0
      RETURN
C
C P is in the triangle (N1,N2,N3).
C
   16 I1 = N1
      I2 = N2
      I3 = N3
      RETURN
C
C All points are collinear.
C
   17 I1 = 0
      I2 = 0
      I3 = 0
      RETURN
      END
      SUBROUTINE TRLIST (NCC,LCC,N,LIST,LPTR,LEND,NROW, NT,
     .                   LTRI,LCT,IER)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
     .        NROW, NT, LTRI(NROW,*), LCT(NCC), IER
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   11/12/94
C
C   This subroutine converts a triangulation data structure
C from the linked list created by subroutine TRMESH or
C TRMSHR to a triangle list.
C
C On input:
C
C       NCC = Number of constraints.  NCC .GE. 0.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).  Refer to
C             subroutine ADDCST.
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       LIST,LPTR,LEND = Linked list data structure defin-
C                        ing the triangulation.  Refer to
C                        subroutine TRMESH.
C
C       NROW = Number of rows (entries per triangle) re-
C              served for the triangle list LTRI.  The value
C              must be 6 if only the vertex indexes and
C              neighboring triangle indexes are to be
C              stored, or 9 if arc indexes are also to be
C              assigned and stored.  Refer to LTRI.
C
C The above parameters are not altered by this routine.
C
C       LTRI = Integer array of length at least NROW*NT,
C              where NT is at most 2N-5.  (A sufficient
C              length is 12N if NROW=6 or 18N if NROW=9.)
C
C       LCT = Integer array of length NCC or dummy array of
C             length 1 if NCC = 0.
C
C On output:
C
C       NT = Number of triangles in the triangulation unless
C            IER .NE. 0, in which case NT = 0.  NT = 2N - NB
C            - 2, where NB is the number of boundary nodes.
C
C       LTRI = NROW by NT array whose J-th column contains
C              the vertex nodal indexes (first three rows),
C              neighboring triangle indexes (second three
C              rows), and, if NROW = 9, arc indexes (last
C              three rows) associated with triangle J for
C              J = 1,...,NT.  The vertices are ordered
C              counterclockwise with the first vertex taken
C              to be the one with smallest index.  Thus,
C              LTRI(2,J) and LTRI(3,J) are larger than
C              LTRI(1,J) and index adjacent neighbors of
C              node LTRI(1,J).  For I = 1,2,3, LTRI(I+3,J)
C              and LTRI(I+6,J) index the triangle and arc,
C              respectively, which are opposite (not shared
C              by) node LTRI(I,J), with LTRI(I+3,J) = 0 if
C              LTRI(I+6,J) indexes a boundary arc.  Vertex
C              indexes range from 1 to N, triangle indexes
C              from 0 to NT, and, if included, arc indexes
C              from 1 to NA = NT+N-1.  The triangles are or-
C              dered on first (smallest) vertex indexes,
C              except that the sets of constraint triangles
C              (triangles contained in the closure of a con-
C              straint region) follow the non-constraint
C              triangles.
C
C       LCT = Array of length NCC containing the triangle
C             index of the first triangle of constraint J in
C             LCT(J).  Thus, the number of non-constraint
C             triangles is LCT(1)-1, and constraint J con-
C             tains LCT(J+1)-LCT(J) triangles, where
C             LCT(NCC+1) = NT+1.
C
C       IER = Error indicator.
C             IER = 0 if no errors were encountered.
C             IER = 1 if NCC, N, NROW, or an LCC entry is
C                     outside its valid range on input.
C             IER = 2 if the triangulation data structure
C                     (LIST,LPTR,LEND) is invalid.  Note,
C                     however, that these arrays are not
C                     completely tested for validity.
C
C Modules required by TRLIST:  None
C
C Intrinsic function called by TRLIST:  ABS
C
C***********************************************************
C
      INTEGER I, I1, I2, I3, ISV, J, JLAST, KA, KN, KT, L,
     .        LCC1, LP, LP2, LPL, LPLN1, N1, N1ST, N2, N3,
     .        NM2, NN
      LOGICAL ARCS, CSTRI, PASS2
C
C Test for invalid input parameters and store the index
C   LCC1 of the first constraint node (if any).
C
      NN = N
      IF (NCC .LT. 0  .OR.  (NROW .NE. 6  .AND.
     .    NROW .NE. 9)) GO TO 12
      LCC1 = NN+1
      IF (NCC .EQ. 0) THEN
        IF (NN .LT. 3) GO TO 12
      ELSE
        DO 1 I = NCC,1,-1
          IF (LCC1-LCC(I) .LT. 3) GO TO 12
          LCC1 = LCC(I)
    1     CONTINUE
        IF (LCC1 .LT. 1) GO TO 12
      ENDIF
C
C Initialize parameters for loop on triangles KT = (N1,N2,
C   N3), where N1 < N2 and N1 < N3.  This requires two
C   passes through the nodes with all non-constraint
C   triangles stored on the first pass, and the constraint
C   triangles stored on the second.
C
C   ARCS = TRUE iff arc indexes are to be stored.
C   KA,KT = Numbers of currently stored arcs and triangles.
C   N1ST = Starting index for the loop on nodes (N1ST = 1 on
C            pass 1, and N1ST = LCC1 on pass 2).
C   NM2 = Upper bound on candidates for N1.
C   PASS2 = TRUE iff constraint triangles are to be stored.
C
      ARCS = NROW .EQ. 9
      KA = 0
      KT = 0
      N1ST = 1
      NM2 = NN-2
      PASS2 = .FALSE.
C
C Loop on nodes N1:  J = constraint containing N1,
C                    JLAST = last node in constraint J.
C
    2 J = 0
      JLAST = LCC1 - 1
      DO 11 N1 = N1ST,NM2
        IF (N1 .GT. JLAST) THEN
C
C N1 is the first node in constraint J+1.  Update J and
C   JLAST, and store the first constraint triangle index
C   if in pass 2.
C
          J = J + 1
          IF (J .LT. NCC) THEN
            JLAST = LCC(J+1) - 1
          ELSE
            JLAST = NN
          ENDIF
          IF (PASS2) LCT(J) = KT + 1
        ENDIF
C
C Loop on pairs of adjacent neighbors (N2,N3).  LPLN1 points
C   to the last neighbor of N1, and LP2 points to N2.
C
        LPLN1 = LEND(N1)
        LP2 = LPLN1
    3     LP2 = LPTR(LP2)
          N2 = LIST(LP2)
          LP = LPTR(LP2)
          N3 = ABS(LIST(LP))
          IF (N2 .LT. N1  .OR.  N3 .LT. N1) GO TO 10
C
C (N1,N2,N3) is a constraint triangle iff the three nodes
C   are in the same constraint and N2 < N3.  Bypass con-
C   straint triangles on pass 1 and non-constraint triangles
C   on pass 2.
C
          CSTRI = N1 .GE. LCC1  .AND.  N2 .LT. N3  .AND.
     .            N3 .LE. JLAST
          IF ((CSTRI  .AND.  .NOT. PASS2)  .OR.
     .        (.NOT. CSTRI  .AND.  PASS2)) GO TO 10
C
C Add a new triangle KT = (N1,N2,N3).
C
          KT = KT + 1
          LTRI(1,KT) = N1
          LTRI(2,KT) = N2
          LTRI(3,KT) = N3
C
C Loop on triangle sides (I1,I2) with neighboring triangles
C   KN = (I1,I2,I3).
C
          DO 9 I = 1,3
            IF (I .EQ. 1) THEN
              I1 = N3
              I2 = N2
            ELSEIF (I .EQ. 2) THEN
              I1 = N1
              I2 = N3
            ELSE
              I1 = N2
              I2 = N1
            ENDIF
C
C Set I3 to the neighbor of I1 which follows I2 unless
C   I2->I1 is a boundary arc.
C
            LPL = LEND(I1)
            LP = LPTR(LPL)
    4       IF (LIST(LP) .EQ. I2) GO TO 5
              LP = LPTR(LP)
              IF (LP .NE. LPL) GO TO 4
C
C   I2 is the last neighbor of I1 unless the data structure
C     is invalid.  Bypass the search for a neighboring
C     triangle if I2->I1 is a boundary arc.
C
            IF (ABS(LIST(LP)) .NE. I2) GO TO 13
            KN = 0
            IF (LIST(LP) .LT. 0) GO TO 8
C
C   I2->I1 is not a boundary arc, and LP points to I2 as
C     a neighbor of I1.
C
    5       LP = LPTR(LP)
            I3 = ABS(LIST(LP))
C
C Find L such that LTRI(L,KN) = I3 (not used if KN > KT),
C   and permute the vertex indexes of KN so that I1 is
C   smallest.
C
            IF (I1 .LT. I2  .AND.  I1 .LT. I3) THEN
              L = 3
            ELSEIF (I2 .LT. I3) THEN
              L = 2
              ISV = I1
              I1 = I2
              I2 = I3
              I3 = ISV
            ELSE
              L = 1
              ISV = I1
              I1 = I3
              I3 = I2
              I2 = ISV
            ENDIF
C
C Test for KN > KT (triangle index not yet assigned).
C
            IF (I1 .GT. N1  .AND.  .NOT. PASS2) GO TO 9
C
C Find KN, if it exists, by searching the triangle list in
C   reverse order.
C
            DO 6 KN = KT-1,1,-1
              IF (LTRI(1,KN) .EQ. I1  .AND.  LTRI(2,KN) .EQ.
     .            I2  .AND.  LTRI(3,KN) .EQ. I3) GO TO 7
    6         CONTINUE
            GO TO 9
C
C Store KT as a neighbor of KN.
C
    7       LTRI(L+3,KN) = KT
C
C Store KN as a neighbor of KT, and add a new arc KA.
C
    8       LTRI(I+3,KT) = KN
            IF (ARCS) THEN
              KA = KA + 1
              LTRI(I+6,KT) = KA
              IF (KN .NE. 0) LTRI(L+6,KN) = KA
            ENDIF
    9       CONTINUE
C
C Bottom of loop on triangles.
C
   10     IF (LP2 .NE. LPLN1) GO TO 3
   11     CONTINUE
C
C Bottom of loop on nodes.
C
      IF (.NOT. PASS2  .AND.  NCC .GT. 0) THEN
        PASS2 = .TRUE.
        N1ST = LCC1
        GO TO 2
      ENDIF
C
C No errors encountered.
C
      NT = KT
      IER = 0
      RETURN
C
C Invalid input parameter.
C
   12 NT = 0
      IER = 1
      RETURN
C
C Invalid triangulation data structure:  I1 is a neighbor of
C   I2, but I2 is not a neighbor of I1.
C
   13 NT = 0
      IER = 2
      RETURN
      END
      SUBROUTINE TRMESH (N,X,Y, LIST,LPTR,LEND,LNEW,IER)
      INTEGER N, LIST(*), LPTR(*), LEND(N), LNEW, IER
      DOUBLE PRECISION    X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/25/91
C
C   This subroutine creates a Delaunay triangulation of a
C set of N arbitrarily distributed points in the plane re-
C ferred to as nodes.  The Delaunay triangulation is defined
C as a set of triangles with the following five properties:
C
C  1)  The triangle vertices are nodes.
C  2)  No triangle contains a node other than its vertices.
C  3)  The interiors of the triangles are pairwise disjoint.
C  4)  The union of triangles is the convex hull of the set
C        of nodes (the smallest convex set which contains
C        the nodes).
C  5)  The interior of the circumcircle of each triangle
C        contains no node.
C
C The first four properties define a triangulation, and the
C last property results in a triangulation which is as close
C as possible to equiangular in a certain sense and which is
C uniquely defined unless four or more nodes lie on a common
C circle.  This property makes the triangulation well-suited
C for solving closest point problems and for triangle-based
C interpolation.
C
C   The triangulation can be generalized to a constrained
C Delaunay triangulation by a call to subroutine ADDCST.
C This allows for user-specified boundaries defining a non-
C convex and/or multiply connected region.
C
C   The operation count for constructing the triangulation
C is close to O(N) if the nodes are presorted on X or Y com-
C ponents.  Also, since the algorithm proceeds by adding
C nodes incrementally, the triangulation may be updated with
C the addition (or deletion) of a node very efficiently.
C The adjacency information representing the triangulation
C is stored as a linked list requiring approximately 13N
C storage locations.
C
C
C   The following is a list of the software package modules
C which a user may wish to call directly:
C
C  ADDCST - Generalizes the Delaunay triangulation to allow
C             for user-specified constraints.
C
C  ADDNOD - Updates the triangulation by appending or
C             inserting a new node.
C
C  AREAP  - Computes the area bounded by a closed polygonal
C             curve such as the boundary of the triangula-
C             tion or of a constraint region.
C
C  BNODES - Returns an array containing the indexes of the
C             boundary nodes in counterclockwise order.
C             Counts of boundary nodes, triangles, and arcs
C             are also returned.
C
C  CIRCUM - Computes the area, circumcenter, circumradius,
C             and, optionally, the aspect ratio of a trian-
C             gle defined by user-specified vertices.
C
C  DELARC - Deletes a boundary arc from the triangulation.
C
C  DELNOD - Updates the triangulation with the deletion of a
C             node.
C
C  EDGE   - Forces a pair of nodes to be connected by an arc
C             in the triangulation.
C
C  GETNP  - Determines the ordered sequence of L closest
C             nodes to a given node, along with the associ-
C             ated distances.  The distance between nodes is
C             taken to be the length of the shortest connec-
C             ting path which intersects no constraint
C             region.
C
C  INTSEC - Determines whether or not an arbitrary pair of
C             line segments share a common point.
C
C  LEFT   - Locates a point relative to a line.
C
C  NEARND - Returns the index of the nearest node to an
C             arbitrary point, along with its squared
C             distance.
C
C  PERMUT - Permutes a vector.
C
C  QSORT  - Defines a permutation by applying a Quick Sort
C             to a vector.
C
C  REORDR - Reorders the nodes, using an order N*log(N)
C             sort, for increased efficiency in TRMESH.
C
C  STORE  - Forces a value to be stored in main memory so
C             that the precision of floating point numbers
C             in memory locations rather than registers is
C             computed.
C
C  TRLIST - Converts the triangulation data structure to a
C             triangle list more suitable for use in a fin-
C             ite element code.
C
C  TRLPRT - Prints the triangle list created by subroutine
C             TRLIST.
C
C  TRMESH - Creates a Delaunay triangulation of a set of
C             nodes.
C
C  TRMSHR - Creates a Delaunay triangulation (more effici-
C             ently than TRMESH) of a set of nodes lying at
C             the vertices of a (possibly skewed) rectangu-
C             lar grid.
C
C  TRPRNT - Prints the triangulation data structure and,
C             optionally, the nodal coordinates.
C
C
C On input:
C
C       N = Number of nodes in the triangulation.  N .GE. 3.
C
C       X,Y = Arrays of length N containing the Cartesian
C             coordinates of the nodes.  (X(K),Y(K)) is re-
C             ferred to as node K, and K is referred to as
C             a nodal index.  The first three nodes must not
C             be collinear.
C
C The above parameters are not altered by this routine.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C On output:
C
C       LIST = Set of nodal indexes which, along with LPTR,
C              LEND, and LNEW, define the triangulation as a
C              set of N adjacency lists -- counterclockwise-
C              ordered sequences of neighboring nodes such
C              that the first and last neighbors of a bound-
C              ary node are boundary nodes (the first neigh-
C              bor of an interior node is arbitrary).  In
C              order to distinguish between interior and
C              boundary nodes, the last neighbor of each
C              boundary node is represented by the negative
C              of its index.
C
C       LPTR = Set of pointers (LIST indexes) in one-to-one
C              correspondence with the elements of LIST.
C              LIST(LPTR(I)) indexes the node which follows
C              LIST(I) in cyclical counterclockwise order
C              (the first neighbor follows the last neigh-
C              bor).
C
C       LEND = Set of pointers to adjacency lists.  LEND(K)
C              points to the last neighbor of node K for
C              K = 1,...,N.  Thus, LIST(LEND(K)) < 0 if and
C              only if K is a boundary node.
C
C       LNEW = Pointer to the first empty location in LIST
C              and LPTR (list length plus one).  LIST, LPTR,
C              LEND, and LNEW are not altered if IER < 0,
C              and are incomplete if IER > 0.
C
C       IER = Error indicator:
C             IER =  0 if no errors were encountered.
C             IER = -1 if N < 3 on input.
C             IER = -2 if the first three nodes are
C                      collinear.
C             IER =  L if nodes L and M coincide for some
C                      M > L.  The linked list represents
C                      a triangulation of nodes 1 to M-1
C                      in this case.
C
C Modules required by TRMESH:  ADDNOD, BDYADD, INSERT,
C                                INTADD, LEFT, LSTPTR,
C                                STORE, SWAP, SWPTST, TRFIND
C
C***********************************************************
C
      LOGICAL LEFT
      DOUBLE PRECISION    STORE
      INTEGER K, KM1, LCC(1), NCC, NN
      DOUBLE PRECISION    EPS, SWTOL
      COMMON/SWPCOM/SWTOL
C
      NN = N
      IF (NN .LT. 3) THEN
        IER = -1
        RETURN
      ENDIF
C
C Compute a tolerance for function SWPTST:  SWTOL = 10*
C   (machine precision)
C
      EPS = 1.
    1 EPS = EPS/2.
        SWTOL = STORE(EPS + 1.)
        IF (SWTOL .GT. 1.) GO TO 1
      SWTOL = EPS*20.
C
C Store the first triangle in the linked list.
C
      IF ( .NOT. LEFT(X(1),Y(1),X(2),Y(2),X(3),Y(3)) ) THEN
C
C   The initial triangle is (1,3,2).
C
        LIST(1) = 3
        LPTR(1) = 2
        LIST(2) = -2
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 1
        LPTR(3) = 4
        LIST(4) = -3
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 2
        LPTR(5) = 6
        LIST(6) = -1
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSEIF ( .NOT. LEFT(X(2),Y(2),X(1),Y(1),X(3),Y(3)) )
     .       THEN
C
C   The initial triangle is (1,2,3).
C
        LIST(1) = 2
        LPTR(1) = 2
        LIST(2) = -3
        LPTR(2) = 1
        LEND(1) = 2
C
        LIST(3) = 3
        LPTR(3) = 4
        LIST(4) = -1
        LPTR(4) = 3
        LEND(2) = 4
C
        LIST(5) = 1
        LPTR(5) = 6
        LIST(6) = -2
        LPTR(6) = 5
        LEND(3) = 6
C
      ELSE
C
C   The first three nodes are collinear.
C
        IER = -2
        RETURN
      ENDIF
C
C Initialize LNEW and add the remaining nodes.  Parameters
C   for ADDNOD are as follows:
C
C   K = Index of the node to be added.
C   KM1 = Index of the starting node for the search in
C         TRFIND and number of nodes in the triangulation
C         on input.
C   NCC = Number of constraint curves.
C   LCC = Dummy array (since NCC = 0).
C
      LNEW = 7
      IF (NN .EQ. 3) THEN
        IER = 0
        RETURN
      ENDIF
      NCC = 0
      DO 2 K = 4,NN
        KM1 = K - 1
        CALL ADDNOD (K,X(K),Y(K),KM1,NCC, LCC,KM1,X,Y,
     .               LIST,LPTR,LEND,LNEW, IER)
        IF (IER .NE. 0) RETURN
    2   CONTINUE
      IER = 0
      RETURN
      END
      SUBROUTINE TRMSHR (N,NX,X,Y, NIT, LIST,LPTR,LEND,LNEW,
     .                   IER)
      INTEGER  N, NX, NIT, LIST(*), LPTR(*), LEND(N), LNEW,
     .         IER
      DOUBLE PRECISION     X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/31/91
C
C   This subroutine creates a Delaunay triangulation of a
C set of N nodes in the plane, where the nodes are the vert-
C ices of an NX by NY skewed rectangular grid with the
C natural ordering.  Thus, N = NX*NY, and the nodes are
C ordered from left to right beginning at the top row so
C that adjacent nodes have indexes which differ by 1 in the
C x-direction and by NX in the y-direction.  A skewed rec-
C tangular grid is defined as one in which each grid cell is
C a strictly convex quadrilateral (and is thus the convex
C hull of its four vertices).  Equivalently, any transfor-
C mation from a rectangle to a grid cell which is bilinear
C in both components has an invertible Jacobian.
C
C   If the nodes are not distributed and ordered as defined
C above, subroutine TRMESH must be called in place of this
C routine.  Refer to subroutine ADDCST for the treatment of
C constraints.
C
C   The first phase of the algorithm consists of construc-
C ting a triangulation by choosing a diagonal arc in each
C grid cell.  If NIT = 0, all diagonals connect lower left
C to upper right corners and no error checking or additional
C computation is performed.  Otherwise, each diagonal arc is
C chosen to be locally optimal, and boundary arcs are added
C where necessary in order to cover the convex hull of the
C nodes.  (This is the first iteration.)  If NIT > 1 and no
C error was detected, the triangulation is then optimized by
C a sequence of up to NIT-1 iterations in which interior
C arcs of the triangulation are tested and swapped if appro-
C priate.  The algorithm terminates when an iteration
C results in no swaps and/or when the allowable number of
C iterations has been performed.  NIT = 0 is sufficient to
C produce a Delaunay triangulation if the original grid is
C actually rectangular, and NIT = 1 is sufficient if it is
C close to rectangular.  Note, however, that the ordering
C and distribution of nodes is not checked for validity in
C the case NIT = 0, and the triangulation will not be valid
C unless the rectangular grid covers the convex hull of the
C nodes.
C
C
C On input:
C
C       N = Number of nodes in the grid.  N = NX*NY for some
C           NY .GE. 2.
C
C       NX = Number of grid points in the x-direction.  NX
C            .GE. 2.
C
C       X,Y = Arrays of length N containing coordinates of
C             the nodes with the ordering and distribution
C             defined in the header comments above.
C             (X(K),Y(K)) is referred to as node K.
C
C The above parameters are not altered by this routine.
C
C       NIT = Nonnegative integer specifying the maximum
C             number of iterations to be employed.  Refer
C             to the header comments above.
C
C       LIST,LPTR = Arrays of length at least 6N-12.
C
C       LEND = Array of length at least N.
C
C On output:
C
C       NIT = Number of iterations employed.
C
C       LIST,LPTR,LEND,LNEW = Data structure defining the
C                             triangulation.  Refer to sub-
C                             routine TRMESH.
C
C       IER = Error indicator:
C             IER = 0 if no errors were encountered.
C             IER = K if the grid element with upper left
C                     corner at node K is not a strictly
C                     convex quadrilateral.  The algorithm
C                     is terminated when the first such
C                     occurrence is detected.  Note that
C                     this test is not performed if NIT = 0
C                     on input.
C             IER = -1 if N, NX, or NIT is outside its valid
C                      range on input.
C             IER = -2 if NIT > 1 on input, and the optimi-
C                      zation loop failed to converge within
C                      the allowable number of iterations.
C                      The triangulation is valid but not
C                      optimal in this case.
C
C Modules required by TRMSHR:  INSERT, LEFT, LSTPTR, NBCNT,
C                                STORE, SWAP, SWPTST
C
C Intrinsic function called by TRMSHR:  ABS
C
C***********************************************************
C
      INTEGER LSTPTR, NBCNT
      LOGICAL LEFT, SWPTST
      DOUBLE PRECISION    STORE
      INTEGER I, ITER, J, K, KP1, LP, LPF, LPK, LPL, LPP,
     .        M1, M2, M3, M4, MAXIT, N0, N1, N2, N3, N4, NI,
     .        NJ, NM1, NN, NNB
      LOGICAL TST
      DOUBLE PRECISION    EPS, SWTOL
      COMMON/SWPCOM/SWTOL
C
C Store local variables and test for errors in input
C   parameters.
C
      NI = NX
      NJ = N/NI
      NN = NI*NJ
      MAXIT = NIT
      NIT = 0
      IF (N .NE. NN  .OR.  NJ .LT. 2  .OR.  NI .LT. 2  .OR.
     .    MAXIT .LT. 0) THEN
        IER = -1
        RETURN
      ENDIF
      IER = 0
C
C Compute a tolerance for function SWPTST:  SWTOL = 10*
C   (machine precision)
C
      EPS = 1.
    1 EPS = EPS/2.
        SWTOL = STORE(EPS + 1.)
        IF (SWTOL .GT. 1.) GO TO 1
      SWTOL = EPS*20.
C
C Loop on grid points (I,J) corresponding to nodes K =
C   (J-1)*NI + I.  TST = TRUE iff diagonals are to be
C   chosen by the swap test.  M1, M2, M3, and M4 are the
C   slopes (-1, 0, or 1) of the diagonals in quadrants 1
C   to 4 (counterclockwise beginning with the upper right)
C   for a coordinate system with origin at node K.
C
      TST = MAXIT .GT. 0
      M1 = 0
      M4 = 0
      LP = 0
      KP1 = 1
      DO 6 J = 1,NJ
        DO 5 I = 1,NI
          M2 = M1
          M3 = M4
          K = KP1
          KP1 = K + 1
          LPF = LP + 1
          IF (J .EQ. NJ  .AND.  I .NE. NI) GO TO 2
          IF (I .NE. 1) THEN
            IF (J .NE. 1) THEN
C
C   K is not in the top row, leftmost column, or bottom row
C     (unless K is the lower right corner).  Take the first
C     neighbor to be the node above K.
C
              LP = LP + 1
              LIST(LP) = K - NI
              LPTR(LP) = LP + 1
              IF (M2 .LE. 0) THEN
                LP = LP + 1
                LIST(LP) = K - 1 - NI
                LPTR(LP) = LP + 1
              ENDIF
            ENDIF
C
C   K is not in the leftmost column.  The next (or first)
C     neighbor is to the left of K.
C
            LP = LP + 1
            LIST(LP) = K - 1
            LPTR(LP) = LP + 1
            IF (J .EQ. NJ) GO TO 3
            IF (M3 .GE. 0) THEN
              LP = LP + 1
              LIST(LP) = K - 1 + NI
              LPTR(LP) = LP + 1
            ENDIF
          ENDIF
C
C   K is not in the bottom row.  The next (or first)
C     neighbor is below K.
C
          LP = LP + 1
          LIST(LP) = K + NI
          LPTR(LP) = LP + 1
C
C   Test for a negative diagonal in quadrant 4 unless K is
C     in the rightmost column.  The quadrilateral associated
C     with the quadrant is tested for strict convexity un-
C     less NIT = 0 on input.
C
          IF (I .EQ. NI) GO TO 3
          M4 = 1
          IF (.NOT. TST) GO TO 2
          IF ( LEFT(X(KP1),Y(KP1),X(K+NI),Y(K+NI),X(K),Y(K))
     .         .OR.  LEFT(X(K),Y(K),X(KP1+NI),Y(KP1+NI),
     .                    X(K+NI),Y(K+NI))
     .         .OR.  LEFT(X(K+NI),Y(K+NI),X(KP1),Y(KP1),
     .                    X(KP1+NI),Y(KP1+NI))
     .         .OR.  LEFT(X(KP1+NI),Y(KP1+NI),X(K),Y(K),
     .                    X(KP1),Y(KP1)) )          GO TO 12
          IF ( SWPTST(KP1,K+NI,K,KP1+NI,X,Y) ) GO TO 2
          M4 = -1
          LP = LP + 1
          LIST(LP) = KP1 + NI
          LPTR(LP) = LP + 1
C
C   The next (or first) neighbor is to the right of K.
C
    2     LP = LP + 1
          LIST(LP) = KP1
          LPTR(LP) = LP + 1
C
C   Test for a positive diagonal in quadrant 1 (the neighbor
C     of K-NI which follows K is not K+1) unless K is in the
C     top row.
C
          IF (J .EQ. 1) GO TO 3
          IF (TST) THEN
            M1 = -1
            LPK = LSTPTR(LEND(K-NI),K,LIST,LPTR)
            LPK = LPTR(LPK)
            IF (LIST(LPK) .NE. KP1) THEN
              M1 = 1
              LP = LP + 1
              LIST(LP) = KP1 - NI
              LPTR(LP) = LP + 1
            ENDIF
          ENDIF
C
C   If K is in the leftmost column (and not the top row) or
C     in the bottom row (and not the rightmost column), then
C     the next neighbor is the node above K.
C
          IF (I .NE. 1  .AND.  J .NE. NJ) GO TO 4
          LP = LP + 1
          LIST(LP) = K - NI
          LPTR(LP) = LP + 1
          IF (I .EQ. 1) GO TO 3
C
C   K is on the bottom row (and not the leftmost or right-
C     most column).
C
          IF (M2 .LE. 0) THEN
            LP = LP + 1
            LIST(LP) = K - 1 - NI
            LPTR(LP) = LP + 1
          ENDIF
          LP = LP + 1
          LIST(LP) = K - 1
          LPTR(LP) = LP + 1
C
C   K is a boundary node.
C
    3     LIST(LP) = -LIST(LP)
C
C   Bottom of loop.  Store LEND and correct LPTR(LP).
C     LPF and LP point to the first and last neighbors
C     of K.
C
    4     LEND(K) = LP
          LPTR(LP) = LPF
    5     CONTINUE
    6   CONTINUE
C
C Store LNEW, and terminate the algorithm if NIT = 0 on
C   input.
C
      LNEW = LP + 1
      IF (MAXIT .EQ. 0) RETURN
C
C Add boundary arcs where necessary in order to cover the
C   convex hull of the nodes.  N1, N2, and N3 are consecu-
C   tive boundary nodes in counterclockwise order, and N0
C   is the starting point for each loop around the boundary.
C
      N0 = 1
      N1 = N0
      N2 = NI + 1
C
C   TST is set to TRUE if an arc is added.  The boundary
C     loop is repeated until a traversal results in no
C     added arcs.
C
    7 TST = .FALSE.
C
C   Top of boundary loop.  Set N3 to the first neighbor of
C     N2, and test for N3 LEFT N1 -> N2.
C
    8   LPL = LEND(N2)
          LP = LPTR(LPL)
          N3 = LIST(LP)
          IF ( LEFT(X(N1),Y(N1),X(N2),Y(N2),X(N3),Y(N3)) )
     .       N1 = N2
          IF (N1 .NE. N2) THEN
C
C   Add the boundary arc N1-N3.  If N0 = N2, the starting
C     point is changed to N3, since N2 will be removed from
C     the boundary.  N3 is inserted as the first neighbor of
C     N1, N2 is changed to an interior node, and N1 is
C     inserted as the last neighbor of N3.
C
            TST = .TRUE.
            IF (N2 .EQ. N0) N0 = N3
            LP = LEND(N1)
            CALL INSERT (N3,LP, LIST,LPTR,LNEW )
            LIST(LPL) = -LIST(LPL)
            LP = LEND(N3)
            LIST(LP) = N2
            CALL INSERT (-N1,LP, LIST,LPTR,LNEW )
            LEND(N3) = LNEW - 1
          ENDIF
C
C   Bottom of loops.  Test for termination.
C
          N2 = N3
          IF (N1 .NE. N0) GO TO 8
        IF (TST) GO TO 7
C
C Terminate the algorithm if NIT = 1 on input.
C
      NIT = 1
      IF (MAXIT .EQ. 1) RETURN
C
C Optimize the triangulation by applying the swap test and
C   appropriate swaps to the interior arcs.  The loop is
C   repeated until no swaps are performed or MAXIT itera-
C   tions have been applied.  ITER is the current iteration,
C   and TST is set to TRUE if a swap occurs.
C
      ITER = 1
      NM1 = NN - 1
    9 ITER = ITER + 1
        TST = .FALSE.
C
C   Loop on interior arcs N1-N2, where N2 > N1 and
C     (N1,N2,N3) and (N2,N1,N4) are adjacent triangles.
C
C   Top of loop on nodes N1.
C
        DO 11 N1 = 1,NM1
          LPL = LEND(N1)
          N4 = LIST(LPL)
          LPF = LPTR(LPL)
          N2 = LIST(LPF)
          LP = LPTR(LPF)
          N3 = LIST(LP)
          NNB = NBCNT(LPL,LPTR)
C
C   Top of loop on neighbors N2 of N1.  NNB is the number of
C                                       neighbors of N1.
C
          DO 10 I = 1,NNB
C
C   Bypass the swap test if N1 is a boundary node and N2 is
C     the first neighbor (N4 < 0), N2 < N1, or N1-N2 is a
C     diagonal arc (already locally optimal) when ITER = 2.
C
            IF ( N4 .GT. 0  .AND.  N2 .GT. N1  .AND.
     .          (ITER .NE. 2  .OR.  ABS(N1+NI-N2) .NE. 1) )
     .          THEN
              IF (SWPTST(N3,N4,N1,N2,X,Y) ) THEN
C
C   Swap diagonal N1-N2 for N3-N4, set TST to TRUE, and set
C     N2 to N4 (the neighbor preceding N3).
C
                CALL SWAP (N3,N4,N1,N2, LIST,LPTR,LEND, LPP)
                TST = .TRUE.
                N2 = N4
              ENDIF
            ENDIF
C
C   Bottom of neighbor loop.
C
            IF (LIST(LPL) .EQ. -N3) GO TO 11
            N4 = N2
            N2 = N3
            LP = LSTPTR(LPL,N2,LIST,LPTR)
            LP = LPTR(LP)
            N3 = ABS(LIST(LP))
   10       CONTINUE
   11     CONTINUE
C
C   Test for termination.
C
        IF (TST  .AND.  ITER .LT. MAXIT) GO TO 9
      NIT = ITER
      IF (TST) IER = -2
      RETURN
C
C Invalid grid cell encountered.
C
   12 IER = K
      RETURN
      END
      SUBROUTINE TRPRNT (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,
     .                   PRNTX)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),
     .        LOUT
      LOGICAL PRNTX
      DOUBLE PRECISION    X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/22/91
C
C   Given a triangulation of a set of points in the plane,
C this subroutine prints the adjacency lists and, option-
C ally, the nodal coordinates on logical unit LOUT.  The
C list of neighbors of a boundary node is followed by index
C 0.  The numbers of boundary nodes, triangles, and arcs,
C and the constraint curve starting indexes, if any, are
C also printed.
C
C
C On input:
C
C       NCC = Number of constraints.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes in the triangulation -- not used
C             unless PRNTX = TRUE.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to subroutine
C                        TRMESH.
C
C       LOUT = Logical unit number for output.  0 .LE. LOUT
C              .LE. 99.  Output is printed on unit 6 if LOUT
C              is outside its valid range on input.
C
C       PRNTX = Logical variable with value TRUE if and only
C               if X and Y are to be printed (to 6 decimal
C               places).
C
C None of the parameters are altered by this routine.
C
C Modules required by TRPRNT:  None
C
C***********************************************************
C
      INTEGER I, INC, K, LP, LPL, LUN, NA, NABOR(30), NB,
     .        ND, NL, NLMAX, NMAX, NODE, NN, NT
      DATA  NMAX/9999/,  NLMAX/60/
C
      NN = N
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C Print a heading and test the range of N.
C
      WRITE (LUN,100) NN
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) THEN
C
C N is outside its valid range.
C
        WRITE (LUN,110)
        GO TO 5
      ENDIF
C
C Initialize NL (the number of lines printed on the current
C   page) and NB (the number of boundary nodes encountered).
C
      NL = 6
      NB = 0
      IF (.NOT. PRNTX) THEN
C
C Print LIST only.  K is the number of neighbors of NODE
C   which are stored in NABOR.
C
        WRITE (LUN,101)
        DO 2 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
C
    1     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 1
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.  Correct the sign of the last
C     neighbor, add 0 to the end of the list, and increment
C     NB.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print the list of neighbors.
C
          INC = (K-1)/14 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,106)
            NL = INC
          ENDIF
          WRITE (LUN,103) NODE, (NABOR(I), I = 1,K)
          IF (K .NE. 14) WRITE (LUN,105)
    2     CONTINUE
      ELSE
C
C Print X, Y, and LIST.
C
        WRITE (LUN,102)
        DO 4 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
    3     K = K + 1
            LP = LPTR(LP)
            ND = LIST(LP)
            NABOR(K) = ND
            IF (LP .NE. LPL) GO TO 3
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(K) = -ND
            K = K + 1
            NABOR(K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, and NABOR.
C
          INC = (K-1)/8 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            WRITE (LUN,106)
            NL = INC
          ENDIF
          WRITE (LUN,104) NODE, X(NODE), Y(NODE),
     .                    (NABOR(I), I = 1,K)
          IF (K .NE. 8) WRITE (LUN,105)
    4     CONTINUE
      ENDIF
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NT = 2*NN - NB - 2
      NA = NT + NN - 1
      IF (NL .GT. NLMAX-6) WRITE (LUN,106)
      WRITE (LUN,107) NB, NA, NT
C
C Print NCC and LCC.
C
    5 WRITE (LUN,108) NCC
      IF (NCC .GT. 0) WRITE (LUN,109) (LCC(I), I = 1,NCC)
      RETURN
C
C Print formats:
C
  100 FORMAT ('1',26X,'ADJACENCY SETS,    N = ',I5//)
  101 FORMAT (1X,'NODE',32X,'NEIGHBORS OF NODE'//)
  102 FORMAT (1X,'NODE',5X,'X(NODE)',8X,'Y(NODE)',
     .        20X,'NEIGHBORS OF NODE'//)
  103 FORMAT (1X,I4,5X,14I5/(1X,9X,14I5))
  104 FORMAT (1X,I4,2E15.6,5X,8I5/(1X,39X,8I5))
  105 FORMAT (1X)
  106 FORMAT ('1')
  107 FORMAT (/1X,'NB = ',I4,' BOUNDARY NODES',5X,
     .        'NA = ',I5,' ARCS',5X,'NT = ',I5,
     .        ' TRIANGLES')
  108 FORMAT (/1X,'NCC =',I3,' CONSTRAINT CURVES')
  109 FORMAT (1X,9X,14I5)
  110 FORMAT (1X,10X,'*** N IS OUTSIDE ITS VALID',
     .        ' RANGE ***')
      END

      SUBROUTINE TROUTP (NCC,LCC,N,X,Y,LIST,LPTR,LEND,LOUT,
     .                   NABOR,NA,NB,NT)
      INTEGER NCC, LCC(*), N, LIST(*), LPTR(*), LEND(N),LOUT,
     .        NABOR(N,N),NA,NB,NT
      DOUBLE PRECISION    X(N), Y(N)
C
C***********************************************************
C
C                                               From TRIPACK
C                                            Robert J. Renka
C                                  Dept. of Computer Science
C                                       Univ. of North Texas
C                                             (817) 565-2767
C                                                   08/22/91
C
C   modified version of TRPRNT
C
C   Given a triangulation of a set of points in the plane,
C this subroutine returns the adjacency lists and, option-
C ally, the nodal coordinates.  The
C list of neighbors of a boundary node is followed by index
C 0.  The numbers of boundary nodes, triangles, and arcs,
C and the constraint curve starting indexes, if any, are
C also returned.
C
C
C On input:
C
C       NCC = Number of constraints.
C
C       LCC = List of constraint curve starting indexes (or
C             dummy array of length 1 if NCC = 0).
C
C       N = Number of nodes in the triangulation.
C           3 .LE. N .LE. 9999.
C
C       X,Y = Arrays of length N containing the coordinates
C             of the nodes in the triangulation -- not used
C             unless PRNTX = TRUE.
C
C       LIST,LPTR,LEND = Data structure defining the trian-
C                        gulation.  Refer to subroutine
C                        TRMESH.
C
C
C None of these parameters are altered by this routine.
C
C Output
C  
C     NABOR
C     NB
C     NA
C     NT
C
C Modules required by TRPRNT:  None
C
C***********************************************************
C
      INTEGER I, INC, K, LP, LPL, LUN, 
     .        ND, NL, NLMAX, NMAX, NODE, NN
      DATA  NMAX/9999/,  NLMAX/60/
C
      NN = N
      LUN = LOUT
      IF (LUN .LT. 0  .OR.  LUN .GT. 99) LUN = 6
C
C test the range of N.
C
      IF (NN .LT. 3  .OR.  NN .GT. NMAX) THEN
C
C N is outside its valid range.
C
c        WRITE (LUN,110)
        GO TO 5
      ENDIF
C
C Initialize NL (the number of lines printed on the current
C   page) and NB (the number of boundary nodes encountered).
C
      NL = 6
      NB = 0

C
C Print X, Y, and LIST.
C
        DO 4 NODE = 1,NN
          LPL = LEND(NODE)
          LP = LPL
          K = 0
    3     K = K + 1
          LP = LPTR(LP)
          ND = LIST(LP)
          NABOR(NODE,K) = ND
          IF (LP .NE. LPL) GO TO 3
          IF (ND .LE. 0) THEN
C
C   NODE is a boundary node.
C
            NABOR(NODE,K) = -ND
            K = K + 1
            NABOR(NODE,K) = 0
            NB = NB + 1
          ENDIF
C
C   Increment NL and print X, Y, and NABOR.
C
          INC = (K-1)/8 + 2
          NL = NL + INC
          IF (NL .GT. NLMAX) THEN
            NL = INC
          ENDIF
    4     CONTINUE
C
C Print NB, NA, and NT (boundary nodes, arcs, and
C   triangles).
C
      NT = 2*NN - NB - 2
      NA = NT + NN - 1
C
C Print NCC and LCC.
C
 5    RETURN
C
C Print formats:
C
  110 FORMAT (1X,10X,'*** N IS OUTSIDE ITS VALID',
     .        ' RANGE ***')
      END
