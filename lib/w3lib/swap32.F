#include <define.h>
       subroutine swap32(a,n)
c
c      reverse order of bytes in integer*4 word, or real*4
c
#ifdef I8R8
       integer*8   a(n)
       character*1 jtemp(8)
#else
       integer*4   a(n)
       character*1 jtemp(4)
#endif
c
       character*1 ktemp
c
       equivalence (jtemp(1),itemp)
c
       save
c
#ifdef I8R8
       do i = 1,n
         itemp    = a(i)
         ktemp    = jtemp(8)
         jtemp(8) = jtemp(1)
         jtemp(1) = ktemp
         ktemp    = jtemp(7)
         jtemp(7) = jtemp(2)
         jtemp(2) = ktemp
         ktemp    = jtemp(6)
         jtemp(6) = jtemp(3)
         jtemp(3) = ktemp
         ktemp    = jtemp(5)
         jtemp(5) = jtemp(4)
         jtemp(4) = ktemp
         a(i)     = itemp
       enddo
#else
       do i = 1,n
         itemp    = a(i)
         ktemp    = jtemp(4)
         jtemp(4) = jtemp(1)
         jtemp(1) = ktemp
         ktemp    = jtemp(3)
         jtemp(3) = jtemp(2)
         jtemp(2) = ktemp
         a(i)     = itemp
       enddo
#endif
       return
       end
