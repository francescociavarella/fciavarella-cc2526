PROGRAM verletvet

! variable declaration

        IMPLICIT NONE

        INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300)
        INTEGER :: k, n
        REAL (KIND=wp) :: m, t
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: xmat
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: vmat
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: fmat
        ALLOCATE ( xmat (11,3) )
        ALLOCATE ( vmat (11,3) )
        ALLOCATE ( fmat (11,3) )

! variable assignement

        k=10
        m=1.0_wp
        t=0.2_wp
        fmat(:,1)=0.0_wp
        fmat(:,2)=0.1_wp
        fmat(:,3)=0.0_wp
        vmat(1,1)=0.0_wp
        vmat(1,2)=0.0_wp
        vmat(1,3)=0.0_wp
        xmat(1,1)=0.0_wp
        xmat(1,2)=0.0_wp
        xmat(1,3)=0.0_wp

! do cilce for calculations

        DO n=1,k
         xmat(n+1,1) = xmat(n,1) + vmat(n,1)*t + ((t**2)*(fmat(n,1)/(2*m)))
         xmat(n+1,2) = xmat(n,2) + vmat(n,2)*t + ((t**2)*(fmat(n,2)/(2*m)))
         xmat(n+1,3) = xmat(n,3) + vmat(n,3)*t + ((t**2)*(fmat(n,3)/(2*m)))
         vmat(n+1,1) = vmat(n,1) + (t*fmat(n,1))/m
         vmat(n+1,2) = vmat(n,2) + (t*fmat(n,2))/m
         vmat(n+1,3) = vmat(n,3) + (t*fmat(n,3))/m
        ENDDO

!print results
        
        OPEN (UNIT=12, FILE="verletvet_outpu.txt", STATUS="replace", ACTION="write")

        DO n=1, k+1
              WRITE (UNIT=12, FMT=*) xmat(n,1), xmat(n,2), xmat(n,3), "", vmat(n,1), vmat(n,2), vmat(n,3)
        ENDDO
  

END PROGRAM verletvet

