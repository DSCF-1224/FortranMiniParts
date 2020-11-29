! ==================================================================================================================================
! 
! # file created #
! 2020/11/19
! 
! # reference #
! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
! 
! ==================================================================================================================================

module mod_random_seed

    ! <module>s to import
    use,     intrinsic :: iso_fortran_env
    use, non_intrinsic :: mod_support_allocate

    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: type
    public  :: typ_random_seed

    ! kind: subroutine
    public  :: random_seed_get
    public  :: random_seed_put
    public  :: show_seed_console



    ! <type>s for this <module>
    type typ_random_seed
        integer, private              :: size
        integer, private, allocatable :: value(:)
    end type typ_random_seed



    ! contained <subroutine>s and <function>s are below
    contains



    subroutine random_seed_get (obj, stat, allowStop)

        ! argument(s) for this <subroutine>
        type(typ_random_seed), intent(inout) :: obj
        integer,               intent(out)   :: stat
        logical,               intent(in)    :: allowStop

        ! STEP.01
        ! get the size of seed values
        call random_seed(size = obj%size)

        ! STEP.02
        ! resize the array to store the seed values
        if ( allocated(obj%value) ) then
            deallocate(obj%value, stat= stat)
            call CheckStatAllocate(stat= stat, allowStop= allowStop)
        end if

        allocate( obj%value(obj%size), stat= stat )
        call CheckStatAllocate(stat= stat, allowStop= allowStop)

        ! STEP.03
        ! get the seed values
        call random_seed( get= obj%value(:) )

        ! STEP.END
        return

    end subroutine random_seed_get



    subroutine random_seed_put (obj, allowStop)

        ! argument(s) for this <subroutine>
        type(typ_random_seed), intent(in) :: obj
        logical,               intent(in) :: allowStop

        ! variable(s) for this <subroutine>
        logical :: validity

        ! STEP.01
        ! check the validity of the given argument
        validity = .true.

        if ( obj%size .le. 0 ) then
            validity = .false.
        else if ( .not. allocated(obj%value) ) then
            validity = .false.
        end if

        if (.not. validity) then
            write(unit=ERROR_UNIT, fmt='(A)') 'An error was detected at subroutine `random_seed`.'
            if (allowStop) stop
        end if

        ! STEP.02
        ! put the stored seed values
        call random_seed(put= obj%value)

    end subroutine random_seed_put



    subroutine show_seed_console (obj)

        ! argument(s) for this <subroutine>
        type(typ_random_seed) , intent(in) :: obj

        write(unit= OUTPUT_UNIT, fmt=*) 'size   > ', obj%size
        write(unit= OUTPUT_UNIT, fmt=*) 'values > ', obj%value(:)

        return

    end subroutine show_seed_console


end module mod_random_seed

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
