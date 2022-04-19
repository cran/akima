      SUBROUTINE BILIIP(X0,Y0,Z0,N0,X,Y,Z,NX,NY,IER)

C     A. Gebhardt <albrecht.gebhardt@aau.at>, Dec. 2016
C
C     Please note that this file is not associated with Akimas
C     interpolation code (and so not under ACM license, so it can be
C     reused without restriction), it is included here just for 
C     comparison with Akimas ACM 760 algorithm for regular gridded
C     data.
C
C     It implements bilinear (in contrast to bicubic as in ACM 760)
C     interpolation, resulting in a continious but not differentiable
C     (at grid lines) surface.

      IMPLICIT NONE
      INTEGER NX,NY,N0,IER
      DOUBLE PRECISION X0(*),Y0(*),Z0(*),X(*),Y(*),Z(NX,*)

      DOUBLE PRECISION XT,YT,X1,Y1
      INTEGER K,I,J

      IER=0
      DO 10 K=1,N0
         DO 20 I=1,NX-1
            DO 30 J=1,NY-1
               IF ((X(I).LE.X0(K)).AND.(X0(K).LE.X(I+1))) THEN
                  IF ((Y(J).LE.Y0(K)).AND.(Y0(K).LE.Y(J+1))) THEN
                     X1=X(I+1)-X(I)
                     Y1=Y(J+1)-Y(J)
                     IF ((X1.EQ.0.0D0).OR.(Y1.EQ.0.0D0)) THEN
                       IER=1
                       RETURN
                     ENDIF
                     XT=(X0(K)-X(I))/X1
                     YT=(Y0(K)-Y(J))/Y1
                     Z0(K)=(1.0D0-YT)*(1.0D0-XT)*Z(I,J)+
     +                    (1.0D0-YT)*XT*Z(I+1,J)+
     +                    YT*(1.0D0-XT)*Z(I,J+1)+
     +                    YT*XT*Z(I+1,J+1)
                  END IF
               END IF
 30         CONTINUE
 20      CONTINUE
 10   CONTINUE

      RETURN
      END
