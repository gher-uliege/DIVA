PROGRAM testVector

USE moduleDIVA
USE moduleVector
USE moduleChrono

Type(Chronometer) :: c1
Type(vector) :: vec1
Type(vector) :: vec2
INTEGER :: dim
dim = 10

 CALL createDIVAContext()
 CALL startChrono()
 CALL vectorSetMemoryIncreaseSize(15)
 CALL vectorCreate(vec1)
 CALL printInformation(vec1)

 CALL vectorSetSize(vec1,dim)
 CALL vectorSetToValue(vec1,10.D+0)
 CALL printInformation(vec1)
 CALL vectorSetSize(vec1,15)
 CALL vectorSetToValue(vec1,11.D+0)
 CALL printInformation(vec1)

 PRINT*,'norm1 ', vectorNorm1(vec1)
 PRINT*,'norm2 ', vectorNorm2(vec1)
 PRINT*,'normInf ', vectorNormInfinity(vec1)
 CALL vectorSqrt(vec1)
 CALL printInformation(vec1)

 CALL vectorInsertValue(vec1,5,16.05D+0)
 
 PRINT*,'sum ', vectorSum(vec1)
 PRINT*,'min ', vectorMin(vec1)
 PRINT*,'max ', vectorMax(vec1)
 CALL vectorSqrt(vec1)
 CALL printInformation(vec1)

 CALL vectorAddValue(vec1,5,16.05D+0)

 PRINT*,'sum ', vectorSum(vec1)
 PRINT*,'min ', vectorMin(vec1)
 PRINT*,'max ', vectorMax(vec1)
 CALL printInformation(vec1)

 CALL vectorScale(vec1,0.2D+0)
 CALL printInformation(vec1)

 CALL vectorCreate(vec2)
 CALL vectorSetSize(vec2,vectorGetSize(vec1))
 CALL vectorSetToValue(vec2,53.D+0)
 CALL printInformation(vec2)
 
 CALL vectorSetSize(vec1,10000000)
 CALL vectorSetSize(vec2,10000000)
 CALL vectorSetToValue(vec1,53.D+0)
 CALL vectorSetToValue(vec2,53.D+0)
 CALL startChrono(c1)
 PRINT*,'dot ', vectorDot(vec1,vec2)
 CALL finishChrono(c1)
 PRINT*,' value(15) ', vectorGetValue(vec1,5)

 CALL vectorDestroy(vec1)
 CALL vectorDestroy(vec2)

 CALL finishChrono()
 CALL printInformationChrono(c1)

 CALL finaliseDIVAContext()

END PROGRAM testVector


