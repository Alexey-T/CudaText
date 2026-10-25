! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 571
  real(kind=8), parameter :: factor = 1685329.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_8887
  function step_8887(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_8887

  ! generated step_2284
  function step_2284(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2284

  ! generated step_444
  function step_444(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_444

  ! generated step_2946
  function step_2946(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2946

  ! generated step_4439
  function step_4439(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4439

  ! generated step_3121
  function step_3121(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3121

  ! generated step_6510
  function step_6510(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6510

  ! generated step_4530
  function step_4530(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4530

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 940
  real(kind=8), parameter :: factor = 8068993.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_1979
  function step_1979(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1979

  ! generated step_1992
  function step_1992(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1992

  ! generated step_9712
  function step_9712(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9712

  ! generated step_3408
  function step_3408(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3408

  ! generated step_9366
  function step_9366(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9366

  ! generated step_1492
  function step_1492(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1492

  ! generated step_9166
  function step_9166(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9166

  ! generated step_4426
  function step_4426(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4426

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 725
  real(kind=8), parameter :: factor = 4707814.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_9972
  function step_9972(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9972

  ! generated step_6872
  function step_6872(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6872

  ! generated step_6848
  function step_6848(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6848

  ! generated step_3884
  function step_3884(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3884

  ! generated step_4104
  function step_4104(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4104

  ! generated step_7484
  function step_7484(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7484

  ! generated step_3204
  function step_3204(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3204

  ! generated step_7852
  function step_7852(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7852

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 540
  real(kind=8), parameter :: factor = 9214496.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_6777
  function step_6777(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6777

  ! generated step_7319
  function step_7319(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7319

  ! generated step_8546
  function step_8546(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_8546

  ! generated step_2300
  function step_2300(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2300

  ! generated step_3038
  function step_3038(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3038

  ! generated step_9797
  function step_9797(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9797

  ! generated step_6095
  function step_6095(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6095

  ! generated step_7603
  function step_7603(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7603

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 332
  real(kind=8), parameter :: factor = 7799801.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_5708
  function step_5708(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_5708

  ! generated step_1979
  function step_1979(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1979

  ! generated step_7472
  function step_7472(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7472

  ! generated step_1674
  function step_1674(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1674

  ! generated step_3356
  function step_3356(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3356

  ! generated step_382
  function step_382(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_382

  ! generated step_1196
  function step_1196(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1196

  ! generated step_5173
  function step_5173(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_5173

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 829
  real(kind=8), parameter :: factor = 6875967.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_2204
  function step_2204(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2204

  ! generated step_4990
  function step_4990(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4990

  ! generated step_9499
  function step_9499(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9499

  ! generated step_368
  function step_368(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_368

  ! generated step_5022
  function step_5022(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_5022

  ! generated step_99
  function step_99(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_99

  ! generated step_1157
  function step_1157(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1157

  ! generated step_1593
  function step_1593(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1593

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 409
  real(kind=8), parameter :: factor = 8399313.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_3136
  function step_3136(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3136

  ! generated step_9042
  function step_9042(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9042

  ! generated step_1911
  function step_1911(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1911

  ! generated step_2389
  function step_2389(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2389

  ! generated step_7924
  function step_7924(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7924

  ! generated step_482
  function step_482(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_482

  ! generated step_725
  function step_725(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_725

  ! generated step_1449
  function step_1449(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1449

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 719
  real(kind=8), parameter :: factor = 7775200.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_480
  function step_480(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_480

  ! generated step_2249
  function step_2249(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2249

  ! generated step_5882
  function step_5882(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_5882

  ! generated step_8386
  function step_8386(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_8386

  ! generated step_2169
  function step_2169(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2169

  ! generated step_6537
  function step_6537(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_6537

  ! generated step_4656
  function step_4656(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4656

  ! generated step_1608
  function step_1608(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1608

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 324
  real(kind=8), parameter :: factor = 6354058.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_9630
  function step_9630(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9630

  ! generated step_7777
  function step_7777(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7777

  ! generated step_8564
  function step_8564(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_8564

  ! generated step_5098
  function step_5098(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_5098

  ! generated step_7042
  function step_7042(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7042

  ! generated step_4843
  function step_4843(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_4843

  ! generated step_3929
  function step_3929(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3929

  ! generated step_2140
  function step_2140(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_2140

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

! Sample Fortran source for lexer benchmarking
module benchmark_mod
  implicit none
  integer, parameter :: max_items = 434
  real(kind=8), parameter :: factor = 3306517.8d0

  type :: entry_t
     character(len=32) :: name
     real(kind=8) :: weight
  end type entry_t

contains

  function score(e) result(s)
    type(entry_t), intent(in) :: e
    real(kind=8) :: s
    if (e%%weight < 0.0d0) then
       s = 0.0d0
    else
       s = e%%weight * factor
    end if
  end function score

  ! generated step_10
  function step_10(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_10

  ! generated step_205
  function step_205(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_205

  ! generated step_3655
  function step_3655(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_3655

  ! generated step_1734
  function step_1734(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1734

  ! generated step_903
  function step_903(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_903

  ! generated step_7583
  function step_7583(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_7583

  ! generated step_1991
  function step_1991(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_1991

  ! generated step_9117
  function step_9117(seed) result(total)
    integer, intent(in) :: seed
    integer :: total, i
    total = 0
    do i = 0, max_items - 1
       if (mod(i, 2) == 0) then
          total = total + i * seed
       else
          total = total - mod(i, 7)
       end if
    end do
  end function step_9117

end module benchmark_mod

program main
  use benchmark_mod
  implicit none
  type(entry_t) :: entries(2)
  entries(1)%%name = "first"
  entries(1)%%weight = 1.5d0
  entries(2)%%name = "second"
  entries(2)%%weight = 2.75d0
  print *, score(entries(1))
end program main

