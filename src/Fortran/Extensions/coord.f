CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C           Latitude - Longitude --> XY Conversion tool                C 
C           =================================================          C
C                                                                      C
C                              M.  RIXEN                               C
C                               MARS 96                                C
C                     GHER - UNIVERSITY OF LIEGE                       C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      PROGRAM COORD
      IMPLICIT NONE


      character*80 tool, direction, input, output
      real*4 lambda,PI

 820  format('CALL TO contour conversion')
 830  format('CALL TO data conversion')
 840  format('CALL TO mesh conversion')
 850  format('CALL TO analyse conversion')
 860  format('CALL TO grid conversion')
 870  format('CALL TO point list conversion')
 880  format('Bad direction')

      read(5,'(A)') tool
      write(6,'(A)') tool

      read(5,'(A)') direction
      write(6,'(A)') direction

      read(5,'(A)') input
      write(6,'(A)') input

      read(5,'(A)') output
      write(6,'(A)') output

      read(5,*) lambda
      write(6,*) lambda
    
      PI=3.14159265359 
      lambda = lambda*2.*PI/360.
 
      if(direction.EQ.'ANGTOXY')THEN

      if(tool.eq.'contour') then
       write(6,820) 
       call ConvertContourAngCart(input, output, lambda)
      endif

      if(tool.eq.'data') then
       write(6,830)
       call ConvertDataAngCart(input, output, lambda)
      endif

      if(tool.eq.'point_list') then
       write(6,870)
       call ConvertPointListAngCart(input, output, lambda)
      endif

      if(tool.eq.'grid') then
       write(6,860)
       call ConvertGridAngCart(input, output, lambda)
      endif

      ELSE IF(direction.EQ.'XYTOANG')THEN

      if(tool.eq.'mesh') then
       write(6,840)
       call ConvertMeshCartAng(input, output, lambda)
      endif

      if(tool.eq.'analyse') then
       write(6,850)
       call ConvertAnalyseCartAng(input, output, lambda)
      endif
      
      ELSE 
        write(6,880)
      ENDIF

      end

C ---------------------------------------------------------------------

      SUBROUTINE ConvertContourAngCart(input, output, lambda)

      IMPLICIT NONE

      CHARACTER*80 input, output
      INTEGER nb_contour, nb_points,i, j, orientation
      REAL*4  longitude, latitude, X, Y, FCT_X, FCT_Y, lambda

      OPEN (UNIT=10,FILE=input)
      OPEN (UNIT=11,FILE=output)
     
      READ(10,*) nb_contour
      WRITE(11,*) nb_contour
     
      DO i=1,nb_contour

       READ(10,*) nb_points, orientation
       WRITE(11,*) nb_points, orientation
       DO j=1,nb_points
        READ(10,*) longitude, latitude
        X=FCT_X(longitude, latitude, lambda)
        Y=FCT_Y(longitude, latitude, lambda)
        WRITE(11,*) X,Y
       ENDDO

      ENDDO
      
      CLOSE(10)
      CLOSE(11)
      END

C ---------------------------------------------------------------------

      SUBROUTINE ConvertDataAngCart(input, output, lambda)

      IMPLICIT NONE

      CHARACTER*80 input, output
      INTEGER IOERR
      REAL*4  longitude, latitude, val, X, Y, FCT_X, FCT_Y, lambda

      OPEN (UNIT=10,FILE=input)
      OPEN (UNIT=11,FILE=output)
    
    
 300  CONTINUE

       READ(10,*,IOSTAT=IOERR) longitude, latitude, val
       IF (IOERR.NE.0) GOTO 400
       X=FCT_X(longitude, latitude, lambda)
       Y=FCT_Y(longitude, latitude, lambda)
       WRITE(11,*) X,Y, val

      GOTO 300

      CLOSE(10)
      CLOSE(11)
      RETURN

  400 IF (IOERR.GT.0) WRITE(6,*) 'READ ERROR ON ', input

      END

C ---------------------------------------------------------------------

      SUBROUTINE ConvertPointListAngCart(input, output, lambda)

      IMPLICIT NONE

      CHARACTER*80 input, output
      INTEGER IOERR
      REAL*4  longitude, latitude, X, Y, FCT_X, FCT_Y, lambda

      OPEN (UNIT=10,FILE=input)
      OPEN (UNIT=11,FILE=output)

 500  CONTINUE

       READ(10,*,IOSTAT=IOERR) longitude, latitude
       IF (IOERR.NE.0) GOTO 600
       X=FCT_X(longitude, latitude, lambda)
       Y=FCT_Y(longitude, latitude, lambda)
       WRITE(11,*) X,Y

      GOTO 500

      CLOSE(10)
      CLOSE(11)
      RETURN

  600 IF (IOERR.GT.0) WRITE(6,*) 'READ ERROR ON ', input

      END

C ---------------------------------------------------------------------

      SUBROUTINE ConvertGridAngCart(input, output, lambda)

      IMPLICIT NONE

      CHARACTER*80 input, output
      INTEGER IOERR, valex, nbx, nby
      REAL*4  xorigin,yorigin,dx,dy, X, Y, FCT_X, FCT_Y, lambda

      OPEN (UNIT=10,FILE=input)
      READ(10,*) xorigin, yorigin
      READ(10,*) dx,dy
      READ(10,*) nbx,nby 
      READ(10,*) valex
      CLOSE(10)

      X = FCT_X(xorigin, yorigin, lambda)
      Y = FCT_Y(xorigin, yorigin, lambda)
      dx = FCT_X(dx, yorigin,lambda)
      dy = FCT_Y(xorigin, dy,lambda)
       
      OPEN (UNIT=11,FILE=output)
      WRITE(11,*) X, Y 
      WRITE(11,*) dx,dy
      WRITE(11,*) nbx,nby
      WRITE(11,*) valex
      CLOSE(11)

      END

C ---------------------------------------------------------------------

      SUBROUTINE ConvertMeshCartAng(input, output, lambda)

      IMPLICIT NONE

      CHARACTER*80 input, input_mh4, output, line
      INTEGER IOERR, node, nb_node, nb_int, nb_mesh, i, length, strlg
      REAL*4  longitude, latitude, X, Y, FCT_LONG, FCT_LAT, lambda

      length=STRLG(output)
      input_mh4=output(1:length)//'.mh4'
      write(6,'(A)') input_mh4

      OPEN (UNIT=10,FILE=input)
      OPEN (UNIT=12,FILE=input_mh4)
      OPEN (UNIT=11,FILE=output)

      READ(12,*) nb_node, nb_int, nb_mesh

      DO i=1, nb_node
       READ(10, *) node, x, y
       longitude=FCT_LONG(x,y, lambda)
       latitude=FCT_LAT(x,y, lambda)
       WRITE(11,*) node,longitude,latitude

      ENDDO

      DO I=1, nb_int
       READ(10,*) node
       WRITE(11,*) node
      ENDDO

      DO I=1,nb_mesh
       READ (10,'(A)') line 
       WRITE(11,'(A)') line
      ENDDO

      CLOSE(10)
      CLOSE(11)
      CLOSE(12)
      RETURN
 
      END

C ---------------------------------------------------------------------

      SUBROUTINE ConvertAnalyseCartAng(input, output, lambda)

      CHARACTER*80 input, output
      INTEGER IMAX,JMAX,KMAX,I,J, K, IPR, NB
      REAL*4  A(500000),VALEX,longitude, latitude
      REAL*4  val, X, Y, FCT_LONG, FCT_LAT, lambda
      REAL*8  c8

      OPEN (UNIT=10,FILE=input, FORM='UNFORMATTED')
      OPEN (UNIT=11,FILE=output, FORM='UNFORMATTED')

      CALL UREADC (10,C8,A,VALEX,IPR,IMAX,JMAX,KMAX,NB)

      DO I=1,IMAX
       DO J=1,JMAX
        DO K=1,KMAX
C         WRITE(6,*) A(I)
        ENDDO
       ENDDO
      ENDDO

      CALL UWRITC (11,C8,A,VALEX,IPR,IMAX,JMAX,KMAX,NB)

      CLOSE(10)
      CLOSE(11)
      RETURN

      END

C ---------------------------------------------------------------------

      REAL*4 FUNCTION FCT_X(longitude, latitude, lambda)
      
      IMPLICIT NONE

      REAL*4 latitude, longitude, lambda 

      FCT_X = longitude*COS(lambda)

      RETURN 

      END

C ---------------------------------------------------------------------

      REAL*4 FUNCTION FCT_Y(longitude, latitude, lambda)
      
      IMPLICIT NONE

      REAL*4 latitude, longitude, lambda

      FCT_Y = latitude

      RETURN

      END

C ---------------------------------------------------------------------

      REAL*4 FUNCTION FCT_LAT(x, y, lambda)

      IMPLICIT NONE

      REAL*4 x, y, lambda

      FCT_LAT = y 

      RETURN

      END

C ---------------------------------------------------------------------

      REAL*4 FUNCTION FCT_LONG(x, y, lambda)

      IMPLICIT NONE

      REAL*4 x, y, lambda

      FCT_LONG = x/COS(lambda) 

      RETURN

      END

C -----------------------------------------------
C --- COUNT LENGTH OF CHAR
C -----------------------------------------------

      INTEGER FUNCTION STRLG (NOM)

      CHARACTER*80 NOM

      STRLG = 1

      DO I = LEN(NOM), 1, -1
         IF (NOM(I:I) .NE. ' ') THEN
            STRLG = I
            GOTO 10
         ENDIF
      ENDDO

10    RETURN

      END

C -----------------------------------------------
      INCLUDE 'ureadc.f'
      INCLUDE 'uwritc.f'
