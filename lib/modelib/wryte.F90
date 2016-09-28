   subroutine wryte(lu,lc,c)                                                 
!$$$  subprogram documentation block                                            
!                                                                               
! subprogram:    wryte       write data out by bytes                            
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31                    
!                                                                               
! abstract: efficiently write unformatted a characeter array.                   
!                                                                               
! program history log:                                                          
!   91-10-31  mark iredell                                                      
!                                                                               
! usage:    call wryte(lu,lc,c)                                                 
!                                                                               
!   input argument list:                                                        
!     lu       - integer unit to which to write                                 
!     lc       - integer number of characters or bytes to write                 
!     c        - characeter (lc) data to write                                  
!                                                                               
! attributes:                                                                   
!   language: cray fortran                                                      
!                                                                               
!$$$                                                                            
   character c(lc)                                                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
   write(lu) c                                                               
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -         
   return                                                                    
   end                                                                       
