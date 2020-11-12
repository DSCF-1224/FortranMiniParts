! ==================================================================================================================================
! 
! # file created #
! 2020/10/25
! 
! # reference #
! https://en.wikipedia.org/wiki/Xorshift
! https://ja.wikipedia.org/wiki/Xorshift
! http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html
! 
! ==================================================================================================================================

module mod_xorshift

    ! <module>s to import
    use, intrinsic :: iso_fortran_env

    ! require all variables to be explicitly declared
    implicit none



    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: subroutine
    public  :: random_seed_xorshift32
    public  :: random_seed_xorshift32_default
    public  :: random_seed_xorshift64
    public  :: random_seed_xorshift64_default
    public  :: random_seed_xorshift128
    public  :: random_seed_xorshift128_default
    public  :: random_number_xorshift32
    public  :: random_number_xorshift64
    private :: random_number_xorshift128_i32
    private :: random_number_xorshift128_r64

    ! kind: interface
    public :: random_number_xorshift128



    ! <type>s for this <module>
    type, public :: typ_state_xorshift32
        integer(INT32), private :: y
    end type typ_state_xorshift32

    type, public :: typ_state_xorshift64
        integer(INT64), private :: x
    end type typ_state_xorshift64

    type, public :: typ_state_xorshift128
        integer(INT32), private :: x
        integer(INT32), private :: y
        integer(INT32), private :: z
        integer(INT32), private :: w
    end type typ_state_xorshift128



    ! <interface>s for this <module>
    interface random_number_xorshift128
        module procedure :: random_number_xorshift128_i32
        module procedure :: random_number_xorshift128_r64
    end interface random_number_xorshift128



    ! contained <subroutine>s and <function>s are below
    contains



    subroutine random_seed_xorshift32 (state, y)

        ! arguments for this <subroutine>
        type(typ_state_xorshift32), intent(inout) :: state
        integer(INT32),             intent(in)    :: y

        state%y = y

        return

    end subroutine random_seed_xorshift32



    subroutine random_seed_xorshift32_default (state)

        ! arguments for this <subroutine>
        type(typ_state_xorshift32), intent(inout) :: state

        call random_seed_xorshift32(state= state, y=-1831433054_INT32)
        return

    end subroutine random_seed_xorshift32_default



    subroutine random_seed_xorshift64 (state, x)

        ! arguments for this <subroutine>
        type(typ_state_xorshift64), intent(inout) :: state
        integer(INT64),             intent(in)    :: x

        state%x = x
        return

    end subroutine random_seed_xorshift64



    subroutine random_seed_xorshift64_default (state)

        ! arguments for this <subroutine>
        type(typ_state_xorshift64), intent(inout) :: state

        call random_seed_xorshift64(state= state, x= 88172645463325252_INT64)
        return

    end subroutine random_seed_xorshift64_default



    subroutine random_seed_xorshift128 (state, x, y, z, w)

        ! arguments for this <subroutine>
        type(typ_state_xorshift128), intent(inout) :: state
        integer(INT32),              intent(in)    :: x, y, z, w

        state%x = x
        state%y = y
        state%z = z
        state%w = w

        return

    end subroutine random_seed_xorshift128



    subroutine random_seed_xorshift128_default (state)

        ! arguments for this <subroutine>
        type(typ_state_xorshift128), intent(inout) :: state

        call random_seed_xorshift128(state= state, x= 123456789_INT32, y= 362436069_INT32, z= 521288629_INT32, w= 88675123_INT32)
        return

    end subroutine random_seed_xorshift128_default



    subroutine random_number_xorshift32 (state, harvest)

        ! arguments for this <subroutine>
        type(typ_state_xorshift32), intent(inout) :: state
        integer(INT32),             intent(out)   :: harvest

        state%y = ieor( state%y, shiftl(i= state%y, shift= 13_INT32) )
        state%y = ieor( state%y, shiftr(i= state%y, shift=  7_INT32) )
        state%y = ieor( state%y, shiftl(i= state%y, shift=  5_INT32) )
        harvest = state%y

        return

    end subroutine random_number_xorshift32



    subroutine random_number_xorshift64 (state, harvest)

        ! arguments for this <subroutine>
        type(typ_state_xorshift64), intent(inout) :: state
        integer(INT64),             intent(out)   :: harvest

        state%x = ieor( state%x, shiftl(i= state%x, shift= 13_INT64) )
        state%x = ieor( state%x, shiftr(i= state%x, shift=  7_INT64) )
        state%x = ieor( state%x, shiftl(i= state%x, shift= 17_INT64) )
        harvest = state%x

        return

    end subroutine random_number_xorshift64



    subroutine random_number_xorshift128_i32 (state, harvest)

        ! arguments for this <subroutine>
        type(typ_state_xorshift128), intent(inout) :: state
        integer(INT32),              intent(out)   :: harvest

        harvest = ieor(state%x, shiftl(i= state%x, shift= 11_INT32))
        state%x = state%y
        state%y = state%z
        state%z = state%w
        state%w = ieor( ieor(state%w, shiftr(i= state%w, shift= 19_INT32)) , ieor(harvest, shiftr(i= harvest, shift= 8_INT32)) )
        harvest = state%w

        return

    end subroutine random_number_xorshift128_i32



    subroutine random_number_xorshift128_r64 (state, harvest)

        ! arguments for this <subroutine>
        type(typ_state_xorshift128), intent(inout) :: state
        real(REAL64),                intent(out)   :: harvest

        ! variables for this <subroutine>
        integer(INT32) :: harvest_int

        call random_number_xorshift128_i32 (state= state, harvest= harvest_int)
        harvest = real(harvest_int, kind=REAL64)

        return

    end subroutine random_number_xorshift128_r64

end module mod_xorshift

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
