subroutine Readxyz()
    implicit none
    
    character(120)          ::  xyz
    integer                 ::  natoms
    integer                 ::  i, j
    real(8), allocatable    ::  atoms(:), coords(:,:)

    open(10, file=xyz, status='unknown')
    read(10,*) natoms
    allocate(atoms(natoms),coords(natoms,3))
    read(10,*) ! skip the second line (next to # of atoms)

    do i=1,natoms
      read(10,*) atoms(i) !, (coords(i,j) (j=1,3))
    enddo

    deallocate(atoms,coords)

    print*, atoms
    print*, coords

    close(10)

end subroutine Readxyz





