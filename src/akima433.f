C     Modified after ACM Algorithm 433 by ThPe
C
C     ALGORITHM 433 COLLECTED ALGORITHMS FROM ACM.
C     ALGORITHM APPEARED IN COMM. ACM, VOL. 15, NO. 10,
C     P. 914.
      SUBROUTINE  INTRPL(L,X,Y,N,U,V,ERR)
      IMPLICIT INTEGER (I-N), DOUBLE PRECISION (A-H,O-Z)
C INTERPOLATION OF A SINGLE-VALUED FUNCTION
C THIS SUBROUTINE INTERPOLATES, FROM VALUES OF THE FUNCTION
C GIVEN AS ORDINATES OF INPUT DATA POINTS IN AN X-Y PLANE
C AND FOR A GIVEN SET OF X VALUES (ABSCISSAS), THE VALUES OF
C A SINGLE-VALUED FUNCTION Y = Y(X).
C THE INPUT PARAMETERS ARE
C     L  = NUMBER OF INPUT DATA POINTS
C          (MUST BE 2 OR GREATER)
C     X  = ARRAY OF DIMENSION L STORING THE X VALUES
C          (ABSCISSAS) OF INPUT DATA POINTS
C          (IN ASCENDING ORDER)
C     Y  = ARRAY OF DIMENSION L STORING THE Y VALUES
C          (ORDINATES) OF INPUT DATA POINTS
C     N  = NUMBER OF POINTS AT WHICH INTERPOLATION OF THE
C          Y VALUE (ORDINATE) IS DESIRED
C          (MUST BE 1 OR GREATER)
C     U  = ARRAY OF DIMENSION N STORING THE X VALUES
C          (ABSCISSAS) OF DESIRED POINTS
C THE OUTPUT PARAMETER IS
C     V  = ARRAY OF DIMENSION N WHERE THE INTERPOLATED Y
C          VALUES (ORDINATES) ARE TO BE DISPLAYED
C   ERR  = ERROR CODE (added by ThPe)
C DECLARATION STATEMENTS
      INTEGER             L, N, ERR
      DOUBLE PRECISION    X(L),Y(L),U(N),V(N)
      EQUIVALENCE         (P0,X3),(Q0,Y3),(Q1,T3)
      DOUBLE PRECISION    M1,M2,M3,M4,M5
      DOUBLE PRECISION    A2,A4
      EQUIVALENCE  (UK,DX),(IMN,X2,A1,M1),(IMX,X5,A5,M5),
     1             (J,SW,SA),(Y2,W2,W4,Q2),(Y5,W3,Q3)
C PRELIMINARY PROCESSING
      L0=L
      LM1=L0-1
      LM2=LM1-1
      LP1=L0+1
      N0=N
      M2=0
      A2=0
      A4=0
      IF(LM2.LT.0)        GO TO 90
      IF(N0.LE.0)         GO TO 91
      DO 11  I=2,L0
        IF(X(I-1)-X(I).LT.0.0D0) GO TO 11
        IF(X(I-1)-X(I).EQ.0.0D0) GO TO 95
        IF(X(I-1)-X(I).GT.0.0D0) GO TO 96
   11   CONTINUE
      IPV=0
C MAIN DO-LOOP
      DO 80  K=1,N0
        UK=U(K)
C ROUTINE TO LOCATE THE DESIRED POINT
        IF(LM2.EQ.0)      GO TO 27
        IF(UK.GE.X(L0))   GO TO 26
        IF(UK.LT.X(1))    GO TO 25
        IMN=2
        IMX=L0
   21   I=(IMN+IMX)/2
        IF(UK.GE.X(I))    GO TO 23
        IMX=I
        GO TO 24
   23   IMN=I+1
   24   IF(IMX.GT.IMN)    GO TO 21
        I=IMX
        GO TO 30
   25   I=1
        GO TO 30
   26   I=LP1
        GO TO 30
   27   I=2
C CHECK IF I = IPV
   30   IF(I.EQ.IPV)      GO TO 70
        IPV=I
C ROUTINES TO PICK UP NECESSARY X AND Y VALUES AND
C          TO ESTIMATE THEM IF NECESSARY
        J=I
        IF(J.EQ.1)        J=2
        IF(J.EQ.LP1)      J=L0
        X3=X(J-1)
        Y3=Y(J-1)
        X4=X(J)
        Y4=Y(J)
        A3=X4-X3
        M3=(Y4-Y3)/A3
        IF(LM2.EQ.0)      GO TO 43
        IF(J.EQ.2)        GO TO 41
        X2=X(J-2)
        Y2=Y(J-2)
        A2=X3-X2
        M2=(Y3-Y2)/A2
        IF(J.EQ.L0)       GO TO 42
   41   X5=X(J+1)
        Y5=Y(J+1)
        A4=X5-X4
        M4=(Y5-Y4)/A4
        IF(J.EQ.2)        M2=M3+M3-M4
        GO TO 45
   42   M4=M3+M3-M2
        GO TO 45
   43   M2=M3
        M4=M3
   45   IF(J.LE.3)        GO TO 46
        A1=X2-X(J-3)
        M1=(Y2-Y(J-3))/A1
        GO TO 47
   46   M1=M2+M2-M3
   47   IF(J.GE.LM1)      GO TO 48
        A5=X(J+2)-X5
        M5=(Y(J+2)-Y5)/A5
        GO TO 50
   48   M5=M4+M4-M3
C NUMERICAL DIFFERENTIATION
   50   IF(I.EQ.LP1)      GO TO 52
        W2=ABS(M4-M3)
        W3=ABS(M2-M1)
        SW=W2+W3
        IF(SW.NE.0.0)     GO TO 51
        W2=0.5
        W3=0.5
        SW=1.0
   51   T3=(W2*M2+W3*M3)/SW
        IF(I.EQ.1)        GO TO 54
   52   W3=ABS(M5-M4)
        W4=ABS(M3-M2)
        SW=W3+W4
        IF(SW.NE.0.0)     GO TO 53
        W3=0.5
        W4=0.5
        SW=1.0
   53   T4=(W3*M3+W4*M4)/SW
        IF(I.NE.LP1)      GO TO 60
        T3=T4
        SA=A2+A3
        T4=0.5*(M4+M5-A2*(A2-A3)*(M2-M3)/(SA*SA))
        X3=X4
        Y3=Y4
        A3=A2
        M3=M4
        GO TO 60
   54   T4=T3
        SA=A3+A4
        T3=0.5*(M1+M2-A4*(A3-A4)*(M3-M4)/(SA*SA))
        X3=X3-A4
        Y3=Y3-M2*A4
        A3=A4
        M3=M2
C DETERMINATION OF THE COEFFICIENTS
   60   Q2=(2.0*(M3-T3)+M3-T4)/A3
        Q3=(-M3-M3+T3+T4)/(A3*A3)
C COMPUTATION OF THE POLYNOMIAL
   70   DX=UK-P0
        V(K)=Q0+DX*(Q1+DX*(Q2+DX*Q3))
   80 CONTINUE
      RETURN
C ERROR EXIT
   90 ERR = 1
      RETURN
   91 ERR = 2
      RETURN
   95 ERR = 6
      RETURN
   96 ERR = 7
      RETURN
      END
