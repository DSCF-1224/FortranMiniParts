! ==================================================================================================================================
! 
! # file created #
! 2020/09/20
! 
! ==================================================================================================================================

module mod_coordinate_cartesian

    ! <module>s to import
    use, intrinsic :: iso_fortran_env



    ! require all variables to be explicitly declared
    implicit none


    ! accessibility of the <subroutine>s and <function>s in this <module>

    ! kind: type
    public :: typ_coordinate_cartesian_2D_R32
    public :: typ_coordinate_cartesian_2D_R64
    public :: typ_coordinate_cartesian_3D_R32
    public :: typ_coordinate_cartesian_3D_R64

    ! kind: function
    private :: add_2D_R32
    private :: add_2D_R64
    private :: add_3D_R32
    private :: add_3D_R64

    private :: norm2_2D_R32
    private :: norm2_2D_R64
    private :: norm2_3D_R32
    private :: norm2_3D_R64

    private :: minus_2D_R32
    private :: minus_2D_R64
    private :: minus_3D_R32
    private :: minus_3D_R64

    private :: sub_2D_R32
    private :: sub_2D_R64
    private :: sub_3D_R32
    private :: sub_3D_R64

    ! kind: interface: operator
    public :: operator(+)
    public :: operator(-)

    ! kind: interface: function
    public :: norm2



    ! <type>s for this <module>

    type typ_coordinate_cartesian_2D_R32
        real(REAL32), public :: x
        real(REAL32), public :: y
    end type typ_coordinate_cartesian_2D_R32

    type typ_coordinate_cartesian_2D_R64
        real(REAL64), public :: x
        real(REAL64), public :: y
    end type typ_coordinate_cartesian_2D_R64

    type typ_coordinate_cartesian_3D_R32
        real(REAL32), public :: x
        real(REAL32), public :: y
        real(REAL32), public :: z
    end type typ_coordinate_cartesian_3D_R32

    type typ_coordinate_cartesian_3D_R64
        real(REAL64), public :: x
        real(REAL64), public :: y
        real(REAL64), public :: z
    end type typ_coordinate_cartesian_3D_R64




    ! <interface>s for this <module>
    interface operator(+)

        ! binary operator
        module procedure :: add_2D_R32
        module procedure :: add_2D_R64
        module procedure :: add_3D_R32
        module procedure :: add_3D_R64

    end interface operator(+)

    interface operator(-)

        ! Unary operator
        module procedure :: minus_2D_R32
        module procedure :: minus_2D_R64
        module procedure :: minus_3D_R32
        module procedure :: minus_3D_R64

        ! binary operator
        module procedure :: sub_2D_R32
        module procedure :: sub_2D_R64
        module procedure :: sub_3D_R32
        module procedure :: sub_3D_R64

    end interface operator(-)

    interface norm2
        module procedure :: norm2_2D_R32
        module procedure :: norm2_2D_R64
        module procedure :: norm2_3D_R32
        module procedure :: norm2_3D_R64
    end interface norm2



    ! contained <subroutine>s and <function>s are below
    contains



    pure function add_2D_R32 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y

    end function add_2D_R32



    pure function add_2D_R64 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y

    end function add_2D_R64



    pure function add_3D_R32 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y
        coordinate_add%z = coordinate_a%z + coordinate_b%z

    end function add_3D_R32



    pure function add_3D_R64 (coordinate_a, coordinate_b) result(coordinate_add)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_add

        coordinate_add%x = coordinate_a%x + coordinate_b%x
        coordinate_add%y = coordinate_a%y + coordinate_b%y
        coordinate_add%z = coordinate_a%z + coordinate_b%z

    end function add_3D_R64



    pure function norm2_2D_R32 (coordinate) result(norm2)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL32) :: norm2

        norm2 = coordinate%x * coordinate%x + coordinate%y * coordinate%y

    end function norm2_2D_R32



    pure function minus_2D_R32 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y

    end function minus_2D_R32



    pure function minus_2D_R64 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y

    end function minus_2D_R64



    pure function minus_3D_R32 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y
        coordinate_minus%z = - coordinate%z

    end function minus_3D_R32



    pure function minus_3D_R64 (coordinate) result(coordinate_minus)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_minus

        coordinate_minus%x = - coordinate%x
        coordinate_minus%y = - coordinate%y
        coordinate_minus%z = - coordinate%z

    end function minus_3D_R64



    pure function norm2_2D_R64 (coordinate) result(norm2)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL64) :: norm2

        norm2 = coordinate%x * coordinate%x + coordinate%y * coordinate%y

    end function norm2_2D_R64



    pure function norm2_3D_R32 (coordinate) result(norm2)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL32) :: norm2

        norm2 = coordinate%x * coordinate%x + coordinate%y * coordinate%y + coordinate%z * coordinate%z

    end function norm2_3D_R32



    pure function norm2_3D_R64 (coordinate) result(norm2)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate

        ! return value of this <function>
        real(REAL64) :: norm2

        norm2 = coordinate%x * coordinate%x + coordinate%y * coordinate%y + coordinate%z * coordinate%z

    end function norm2_3D_R64



    pure function sub_2D_R32 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R32) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_2D_R32



    pure function sub_2D_R64 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_2D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_2D_R64) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_2D_R64



    pure function sub_3D_R32 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R32), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R32) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_3D_R32



    pure function sub_3D_R64 (coordinate_a, coordinate_b) result(coordinate_sub)

        ! arguments for this <function>
        type(typ_coordinate_cartesian_3D_R64), intent(in) :: coordinate_a, coordinate_b

        ! return value of this <function>
        type(typ_coordinate_cartesian_3D_R64) :: coordinate_sub

        coordinate_sub = - coordinate_b + coordinate_a

    end function sub_3D_R64


end module mod_coordinate_cartesian

! ==================================================================================================================================
! EOF
! ==================================================================================================================================
