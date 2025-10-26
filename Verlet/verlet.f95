PROGRAM verlet
        IMPLICIT NONE
        ! Variable declaration
        INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300) !comando che genera un parametro che ci permette di avere gli integer come numeri in double precision
        INTEGER :: i, k
        REAL (KIND=wp) :: t, m, vx, vy, vz, fx, fy, fz, x, y, z, xf, yf, zf, vxf, vyf, vzf
        
       ! Variable assignement
       k=10
       m=1.0_wp
       t=0.2_wp
       fx=0.0_wp
       fy=0.1_wp
       fz=0.0_wp
       vx=0.0_wp
       vy=0.0_wp
       vz=0.0_wp
       x=0.0_wp
       y=0.0_wp
       z=0.0_wp 

       !cicle
       DO i=1,k         
         xf=x+t*vx+((t**2)*(fx/(2*m)))
         yf=y+t*vy+((t**2)*(fy/(2*m)))
         zf=z+t*vz+((t**2)*(fz/(2*m)))
         vxf=vx+(t*fx)/m
         vyf=vy+(t*fy)/m
         vzf=vz+(t*fz)/m
         PRINT *, xf, yf, zf, vxf, vyf, vzf
         x=xf
         y=yf
         z=zf
         vx=vxf
         vy=vyf
         vz=vzf
       
       ENDDO
     
END PROGRAM verlet
