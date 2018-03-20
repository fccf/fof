module fof_test_data

  implicit none


  integer, parameter :: is = 1, &
                        iv(*) = [1,2,3,4], &
                        im(*,*) = reshape([1,2,3,4],[2,2])

  real,    parameter :: rs = 1.0, &
                        rv(*) = [1.,2.,3.,4.], &
                        rm(*,*) = reshape([1.,2.,3.,4.],[2,2])

  logical, parameter :: ls = .TRUE., &
                        lv(*) = [.TRUE.,.FALSE.,.FALSE.,.TRUE.], &
                        lm(*,*) = reshape([.TRUE.,.FALSE.,.FALSE.,.TRUE.],[2,2])

  character(*), parameter :: cs = 'xxxxxxx',&
                             cv(*) = ['ab','cd','ef','gh'],&
                             cm(*,*) = reshape(['ab','cd','ef','gh'],[2,2])


end module fof_test_data
