TYPE vector
    LOGICAL :: isAllocated
    INTEGER :: nbOfData
    INTEGER :: allocatedSize
    REAL*8, DIMENSION(:), POINTER :: values
END TYPE
