
!****************************************************************

program P3D

!****************************************************************
include 'link_fnl_shared.h' 
use P3D_general             

implicit none               
integer :: i1,i2,eq(3),ieig,iopt
real(8) :: omega
character :: rec*200        
logical :: flok

!
! input phase
!

inquire(file='..\inputpiezo\analysis.txt',exist=flok)       
if(.not.flok) then
 write(*,*) 'File analysis.txt inesistente '
 stop                             
else
 open(10,file='..\inputpiezo\analysis.txt',status='old') 
endif

read(10,*) rec
read(10,*) file_name 
close(10)

gid_name=file_name 
write(*,*) file_name(1:LEN_TRIM(file_name))

if(rec(1:4)=='gmsh') then

 file_name='..\inputpiezo\gmsh\'//file_name
 call P3D_inputGMSH() 

elseif(rec(1:6)=='comsol') then
 file_name='..\inputpiezo\comsol\'//file_name
 call P3D_inputCOMSOL() 

else

 write(*,*) 'option not available in input'
 stop

endif

!file_name='..\inputpiezo\gmsh\'//file_name
!call P3D_inputGMSH() 

write(*,*) 'Number elements',number_elements

call gauss_tria() ! pesi e punti di gauss    
!call gauss_T10()  ! pesi e punti di gauss  

call gauss_wedge()
!call gauss_quad()  


i1=index(gid_name,'.msh')-1
res_name=gid_name(1:i1)//'.res'
eig_name=gid_name(1:i1)//'.dat'
freq_name=gid_name(1:i1)//'_freq.txt'


! GID output: results
open(11,file=res_name,status='unknown')
write(11,*) 'GiD Post Results File 1.0'
close(11)

iopt=1


if(iopt==0) then

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  direct solution
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 call P3D_system()

elseif(iopt==1) then
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  real eigenvalue analysis
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
 ! solves coupled eigenproblem
 call P3D_eigen_shift_electrode()

 ! prepares output for pmut
 call P3D_pmut()  

elseif(iopt==2) then
    
 ! solves decoupled eigenproblem
 call P3D_eigen_shift_decoupled()
 
 ! prepares output for pmut
 call P3D_pmut_decoupl()

endif 

end

    
