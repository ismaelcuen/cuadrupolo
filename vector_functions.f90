!!Este modulo es para hacer algunas operaciones de vectores que
!!pueden ser de utilidad
MODULE vector_functions
USE charged_objects
IMPLICIT NONE

CONTAINS
!!Esta función sirve para rellenar los elementos del arreglo sin
!!declaración explícita de los índices.

FUNCTION vector ( x0 , y0 , z0 )
IMPLICIT NONE
REAL ( d ) :: x0 , y0 , z0
REAL ( d ) , DIMENSION ( 3 ) :: vector
vector ( 1 ) = x0
vector ( 2 ) = y0
vector ( 3 ) = z0
END FUNCTION vector

!!Función construida para calcular la magnitud de un vector de 3D
FUNCTION mag ( x0 )
IMPLICIT NONE
REAL ( d ) , DIMENSION ( 3 ) :: x0
REAL ( d ) :: mag
mag = SQRT ( x0 ( 1 ) ** 2 + x0 ( 2 ) ** 2 + x0 ( 3 ) **2 )
END FUNCTION mag

FUNCTION cross_product(a,b)
    IMPLICIT NONE

    !----------------------------------------------------------------------------------------------                                                                             
    !Esta función calcula el producto cruz entre dos vectores usando el pseudotensor de Levi Civita                                                                             
    !----------------------------------------------------------------------------------------------                                                                             

    !Declaración de variables                                                                                                                                                   
    INTEGER,PARAMETER::q=SELECTED_REAL_KIND(16) !kind de 16                                                                                                                     
    REAL(q),INTENT(IN),DIMENSION(3)::a,b !vectores con los que se calculará el producto cruz                                                                                    
    REAL(q),DIMENSION(3)::cross_product !resultado del producto cruz                                                                                                            
    INTEGER,DIMENSION(3,3,3)::e !pseudotensor de Levi Civita                                                                                                                    
    INTEGER::i,j,k

    !inicializacion del pseudotensor                                                                                                                                            
    e = 0
    e(1,2,3) = 1    e(2,3,1) = 1
    e(3,1,2) = 1
    e(3,2,1) = -1
    e(1,3,2) = -1
    e(2,1,3) = -1

    ciclo_indice_e: DO i=1, 3
       ciclo_incdice_a: DO j=1, 3
          IF (i/=j) THEN
             ciclo_indice_b: DO k=1, 3
		IF (i/=k .OR. j/=k) THEN
                   cross_product(i) = cross_product(i) + e(i,j,k)*a(j)*b(k)
                END IF
             END DO ciclo_indice_b
          END IF
       END DO ciclo_incdice_a
    END DO ciclo_indice_e
  END FUNCTION cross_product


END MODULE vector_functions
