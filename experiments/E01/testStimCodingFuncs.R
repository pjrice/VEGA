

# visualize individual stimuli coding results
# me v1
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p017_s15_v1.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p009_s28_v1.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p001_s31_v1.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p004_s40_v1.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p012_s55_v1.xlsx')

# me v2
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p017_s15_v2.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p009_s28_v2.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p001_s31_v2.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p004_s40_v2.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p012_s55_v2.xlsx')

# Mike
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s15.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s28.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s31.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s40.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s55.xlsx')

# Linda
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Linda/p017_s15.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Linda/p009_s28.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Linda/p001_s31.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Linda/p004_s40.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Linda/p012_s55.xlsx')

# Ariana
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s15.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s28.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s31.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s40.xlsx')
visualizeStimCoding('/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s55.xlsx')


# visualize comparisons of stimuli coding results
#s15
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p017_s15_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s15.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p017_s15.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s15.xlsx')
compareStimCoding(filepaths,checkFiles=FALSE)

#s28
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p009_s28_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s28.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p009_s28.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s28.xlsx')
compareStimCoding(filepaths,checkFiles=FALSE)

#s31 - issues w/ two colors in one cell, order of two colors in one cell
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p001_s31_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s31.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p001_s31.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s31.xlsx')
compareStimCoding(filepaths,checkFiles=FALSE)

#s40
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p004_s40_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s40.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p004_s40.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s40.xlsx')
compareStimCoding(filepaths,checkFiles=FALSE)

#s55
filepaths = c('/home/ausmanpa/Desktop/stimCodingComp/Patrick/p012_s55_v1.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Mike/mdbMatrix_s55.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Linda/p012_s55.xlsx',
              '/home/ausmanpa/Desktop/stimCodingComp/Ariana/Ariana_Matrix_s55.xlsx')
compareStimCoding(filepaths,checkFiles=FALSE)