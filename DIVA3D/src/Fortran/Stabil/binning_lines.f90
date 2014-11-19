!!! Sylvain Watelet !!!
!!! 22/07/2014 !!!
!!! Binning lines in input data file thanks to their line number !!!

Program binning_lines
implicit none

integer::i,j,k,n,y,z,old_col1,old_col2,a,lines,bin,nx,ny,samepixel,first_val,bigsquare,smallsquare
integer::inlon,inlat
real(kind=8)::xori,yori,dx,dy,lonmin,latmin,sumweight,first_wgt
 character(len=150)::line,var,cxori,cyori,cnx,cny,cdx,cdy
integer(kind=4),allocatable,dimension(:)::col1,col2,var5,var6,var7,var8,vec
real(kind=8),allocatable,dimension(:)::var1,var2,var3,var4,col3,col4,weight
 character(len=50),allocatable,dimension(:)::var9,var10
real(kind=8)::ave_var1,ave_var2,ave_var3,ave_var4
 character(len=50)::ave_var5,ave_var6,ave_var7,ave_var8
 character(len=100)::ave_var9,ave_var10
 character(len=50)::cvar5,cvar6,cvar7,cvar8
 character(len=100)::cvar9,cvar10

call get_command_argument(1,var)
write(*,*) var
call get_command_argument(2,cxori)
read(cxori,*) xori
write(*,*) xori
call get_command_argument(3,cyori)
read(cyori,*) yori
write(*,*) yori
call get_command_argument(4,cnx)
read(cnx,*) nx
write(*,*) nx
call get_command_argument(5,cny)
read(cny,*) ny
write(*,*) ny
call get_command_argument(6,cdx)
read(cdx,*) dx
write(*,*) dx
call get_command_argument(7,cdy)
read(cdy,*) dy
write(*,*) dy

open(10,file=trim(var),status="unknown")
open(11,file="./input/bidon",status="unknown")
!open(12,file="./input/tmpfile",status="unknown")
!
!write(*,*) "counting neighbours..."
!j=0
!Do while (.true.)
!	j=j+1
!	read(12,*,end=1014)
!Enddo
!1014 continue
!j=j-1
!
!allocate(col1(1:j),col2(1:j),vec(1:2*j))
!col1(:)=0
!col2(:)=0
!vec(:)=0
!
!rewind(12)
!
!write(*,*) "reading line numbers to bin..."
!k=0
!Do k=1,j
!	read(12,*) col1(k),col2(k)
!Enddo

write(*,*) "checking input file length..."
j=0
Do while (.true.)
	j=j+1
	read(10,*,end=1024)
Enddo
1024 continue
j=j-1
lines=j

allocate(col1(1:4*j),col2(1:4*j),col3(1:4*j),col4(1:4*j),vec(1:j),weight(1:j))
allocate(var1(1:j),var2(1:j),var3(1:j),var4(1:j),var5(1:j),var6(1:j),var7(1:j),var8(1:j),var9(1:j),var10(1:j))
var1(:)=0.
var2(:)=0.
var3(:)=0.
var4(:)=0.
var5(:)=0
var6(:)=0
var7(:)=0
var8(:)=0
var9(:)="0"
var10(:)="0"

write(*,*) "reading lines..."
rewind(10)
n=0
Do while (.true.)
	n=n+1
!	write(*,*) n
!	read(10,"(a)",end=1015) line
	read(10,*,end=1015) var1(n),var2(n),var3(n),var4(n),var5(n),var6(n),var7(n),var8(n),var9(n),var10(n)
!	write(11,'(3(f0.4,a),f0.1,a,4(i2,a),3a)') var1(n),char(9),var2(n),char(9),var3(n),char(9),var4(n),char(9),var5(n),char(9),var6(n) &
!& ,char(9),var7(n),char(9),var8(n),char(9),trim(var9(n)),char(9),trim(var10(n))

Enddo

1015 continue

write(*,*) "creating the bins..."

!------------------------------------------------
! Creating the bins 
! (data in current pixel + next to this one
! (3 (4*0.5+4*0.25) other pixels))
!------------------------------------------------

lonmin=xori
z=1
Do i=1,nx
	write(*,'(a,f6.2)') "longitude:",lonmin

	latmin=yori
	Do j=1,ny
		samepixel=0
		Do n=1,lines
			bigsquare=0
			smallsquare=0
			inlon=0
			inlat=0
			! big square (4 pixels)
			If ((var1(n)>lonmin-dx/2.).and.(var1(n)<=lonmin+dx+dx/2.)) then
				If ((var2(n)>latmin-dy/2.).and.(var2(n)<=latmin+dy+dy/2.)) then
					bigsquare=1
					If ((var1(n)>lonmin).and.(var1(n)<=lonmin+dx)) inlon=1
					If ((var2(n)>latmin).and.(var2(n)<=latmin+dy)) inlat=1
					If (samepixel==0) then
						first_val=n
						samepixel=1
						If ((var1(n)>lonmin).and.(var1(n)<=lonmin+dx)) then
							If ((var2(n)>latmin).and.(var2(n)<=latmin+dy)) then
								bigsquare=0
								smallsquare=1
							Endif
						Endif
						If (smallsquare==1) then
						first_wgt=1.
						else
						 If (inlon==1) then
					         first_wgt=(dy/2)/abs(var1(n)-(latmin+dy/2.))
						 else if (inlat==1) then
						 first_wgt=(dx/2.)/abs(var1(n)-(lonmin+dx/2.))
						 else
						 first_wgt=((dx/2.)/abs(var1(n)-(lonmin+dx/2.))+(dy/2)/abs(var1(n)-(latmin+dy/2.)))/2.
						 Endif
						 If (first_wgt<0.01) first_wgt=0.01
						Endif
						cycle
					Endif
					col1(z)=first_val
					col2(z)=n
					col3(z)=first_wgt
					! small square (1 pixel)
					If ((var1(n)>lonmin).and.(var1(n)<=lonmin+dx)) then
						If ((var2(n)>latmin).and.(var2(n)<=latmin+dy)) then
							smallsquare=1
							bigsquare=0
							col4(z)=1.
							z=z+1
						Endif
					Endif
					
					! rest of the square (4*0.5+4*0.25=3 pixels)
					If (bigsquare==1) then
					 If (inlon==1) then
					 col4(z)=(dy/2)/abs(var1(n)-(latmin+dy/2.))
					 else if (inlat==1) then
					 col4(z)=(dx/2.)/abs(var1(n)-(lonmin+dx/2.))
					 else
					 col4(z)=((dx/2.)/abs(var1(n)-(lonmin+dx/2.))+(dy/2)/abs(var1(n)-(latmin+dy/2.)))/2.
					 Endif
					 If (col4(z)<0.01) col4(z)=0.01
					z=z+1
					Endif
				Endif
			Endif
		Enddo
		latmin=latmin+dy
	Enddo
	lonmin=lonmin+dx
Enddo

write(*,*) "binning lines..."

!------------------------------------------------
! Checking for bins
!------------------------------------------------

n=0
z=1
old_col1=0
old_col2=0
Do while (.true.)
	n=n+1
	if (n>lines) exit

	If (n==col1(z).or.n==col2(z)) then
		If (col1(z)/=old_col1) then
		old_col1=col1(z)
		vec(:)=0
		a=1
			Do while (col1(z)==old_col1)
			vec(a)=col2(z)
			weight(a)=col4(z)
			a=a+1
			z=z+1
			Enddo
		vec(a)=col1(z-1) ! now vec contains the indexes of one bin.
		weight(a)=col3(z-1) ! now weight contains the weights of one bin.
		Endif

!------------------------------------------------
! Averaging + weighting
!------------------------------------------------
	
	ave_var1=0.
	ave_var2=0.
	ave_var3=0.
	ave_var4=0.
	sumweight=0.
	Do i=1,lines  !j*2 (why ?)
		if (vec(i)==0) exit			
		ave_var1=ave_var1+var1(vec(i))*weight(i)
		ave_var2=ave_var2+var2(vec(i))*weight(i)
		ave_var3=ave_var3+var3(vec(i))*weight(i)
		ave_var4=ave_var4+var4(vec(i))*weight(i)
		sumweight=sumweight+weight(i)
	Enddo
	ave_var1=ave_var1/sumweight
	ave_var2=ave_var2/sumweight
	ave_var3=ave_var3/sumweight
	ave_var4=ave_var4/sumweight

	write(ave_var5,*) var5(vec(1))
	write(ave_var6,*) var6(vec(1))
	write(ave_var7,*) var7(vec(1))
	write(ave_var8,*) var8(vec(1))
	write(ave_var9,*) var9(vec(1))
	write(ave_var10,*) var10(vec(1))


!------------------------------------------------
! Writing the bins in output file
!------------------------------------------------

	write(11,'(f0.4,x,f0.3,x,f0.4,x,f0.1,x,4(a,x),a,x,a)') ave_var1,ave_var2,ave_var3,ave_var4,trim(adjustl(ave_var5)), &
& trim(adjustl(ave_var6)),trim(adjustl(ave_var7)),trim(adjustl(ave_var8)),trim(adjustl(ave_var9)),trim(adjustl(ave_var10))
	Endif

!------------------------------------------------
! Checking if the current line is in a bin
!------------------------------------------------

	bin=0

	Do i=1,j
	If ((n==col1(i)).or.(n==col2(i))) then
	bin=1
	Endif
	Enddo

!------------------------------------------------
! Copying single observations in output file
!------------------------------------------------
	
	write(cvar5,*) var5(n)
	write(cvar6,*) var6(n)
	write(cvar7,*) var7(n)
	write(cvar8,*) var8(n)
	write(cvar9,*) var9(n)
	write(cvar10,*) var10(n)	
	
	If (bin==0) then
	write(11,'(f0.4,x,f0.3,x,f0.4,x,f0.1,x,4(a,x),a,x,a)') var1(n),var2(n),var3(n),var4(n),trim(adjustl(cvar5)), &
trim(adjustl(cvar6)),trim(adjustl(cvar7)),trim(adjustl(cvar8)),trim(adjustl(cvar9)),trim(adjustl(cvar10))
	Endif

Enddo

write(*,*) "end"

 close(10)
 close(11)
! close(12)

End program
