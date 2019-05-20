MODULE charged_objects
IMPLICIT NONE
SAVE
!!Constantes importantes para el programa
!!Kind con una precisi´on de 16 para mayor resoluci´on en el sistema.
INTEGER , PARAMETER :: d = SELECTED_REAL_KIND ( 16 )
!!Introduciendo las unidades naturales del sistema
REAL(d),PARAMETER::pi=3.14159265358979_d !!Pi con este Kind
REAL(d),PARAMETER::k=9E9_d !!K con este Kind
!!Tipo publico para producir objetos del tipo planeta
TYPE,PUBLIC::charged_particles
REAL(d),DIMENSION(3)::pos !!Posici´on en el tiempo t
REAL(d),DIMENSION(3)::mom !!Momento Lineal en el tiempo t
REAL(d),DIMENSION(3)::vel !!Velocidad en el tiempo t
REAL(d),DIMENSION(3)::Felec !!Fuerza el´ectrica en el tiempo t
REAL(d),DIMENSION(3)::Eelec !!Campo el´ectrico en el tiempo t
REAL ( d ) :: mass !!Masa de la particula cargada
REAL ( d ) :: pos0 !!Posici´on inicial de la particula
REAL ( d ) :: q !!Carga de la particula
REAL ( d ) :: v0 !!Rapidez inicial de la particula
END TYPE charged_particles
!!Tipo publico para producir objetos del tipo estrella
TYPE, PUBLIC :: data
REAL ( d ) :: ttot !!Tiempo total de la simulacion
REAL ( d ) :: dt !!Tama~no de paso
REAL ( d ) :: N_step !!Cantidad de pasos
REAL ( d ), DIMENSION ( 3 ) :: Fenet !!Fuerza el´ectrica neta
REAL ( d ), DIMENSION ( 3 ) :: pos2 !!Vector donde se guarda la
!!diferencia de posici´on entre los objetos
END TYPE data
END MODULE charged_objects
