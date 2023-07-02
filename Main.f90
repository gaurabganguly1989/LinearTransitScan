program Main
    implicit none
    
    integer                         ::  i, j
    integer                         ::  natoms
    character(len=120)              ::  comment
    character(len=6), allocatable   ::  atoms(:)
    real(8), allocatable            ::  icoords(:,:), fcoords(:,:), diff(:,:), delta(:,:)
    integer, parameter              ::  nsteps=20

    open(11, file='cbd_ini.xyz', status='unknown')
    open(12, file='cbd_fin.xyz', status='unknown')
    read(11,*) natoms
    read(11,*) ! skip the line
    read(12,*) ! skip the line
    read(12,*) ! skip the line

    allocate(atoms(natoms),icoords(natoms,3),fcoords(natoms,3),diff(natoms,3),delta(natoms,3))

    do i=1,natoms
      read(11,*)   atoms(i), icoords(i,1:3)
    enddo

    do i=1,natoms
      read(12,*)   atoms(i), fcoords(i,1:3)
    enddo

    do i=1,natoms
      do j=1,3
        diff(i,j) = fcoords(i,j)-icoords(i,j)
      enddo
    enddo
    
    delta = diff/nsteps

   !write(6,'(a,15x,a,30x,a)')  'atoms','initial_coords', 'final_coords'
   !do i=1,natoms
   !  write(6,'(a,3f10.4,6x,3f10.4,6x,3f10.4,6x,3f10.4)')   atoms(i), icoords(i,:), fcoords(i,:), diff(i,1:3), delta(i,:) 
   !enddo
   !write(6,*)


    do i=0,nsteps
      write(6,'(a,i3)')  'Step #', i
      do j=1,natoms
        write(6,'(a,3f10.4)')  atoms(j), icoords(j,1) + i*delta(j,1), icoords(j,2) + i*delta(j,2), icoords(j,3) + i*delta(j,3)
      enddo
      write(6,*)
    enddo

    deallocate(atoms,icoords,fcoords,diff,delta)

    close(11)
    close(12)

end program Main





