! ==================================================================================================================================
! 
! # file created #
! 2020/09/27
! 
! ==================================================================================================================================

module mod_support_allocate

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: subroutine
    public :: CheckStatAllocate



    ! contained <subroutine>s and <function>s are below
    contains



    subroutine CheckStatAllocate (stat, allowStop)

        ! arguments for this <subroutine>
        integer(INT32), intent(in) :: stat
        logical,        intent(in) :: allowStop

        if (stat /= 0_INT32) then

            write(unit=ERROR_UNIT, fmt='(A)') 'An allocating error was detected.'

            if (allowStop) stop

        end if

        return

    end subroutine CheckStatAllocate

end module mod_support_allocate

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
