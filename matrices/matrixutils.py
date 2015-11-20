#!/usr/bin/env python

import io, numpy, scipy, struct, os
from scipy import io as ios
from copy import deepcopy
import matplotlib.pyplot as plot
import urllib, tarfile

dramBase=0x8000100

seyrekBase = "/home/maltanar/seyrek"
# directory for downloads
downloadDir= seyrekBase + "/matrices/download"
# source directory for matrices (in Matrix Market format)
localRoot= seyrekBase + "/matrices/localmtx"
# converted matrices will be outputted here
outputBase = seyrekBase + "/matrices"

testSuite=["Williams/pdb1HYS", "Williams/consph", "Williams/cant", 
           "Boeing/pwtk", "Bova/rma10", "QCD/conf5_4-8x8-05", "DNVS/shipsec1", 
           "Williams/mac_econ_fwd500", "Williams/cop20k_A", 
           "Williams/webbase-1M", "Williams/mc2depi", "Hamm/scircuit"]


# prepare all matrices in the test suite
def prepareTestSuite():
    map(lambda x: prepareUFLMatrix(x), testSuite)

# given the full name of a University of Florida matrix; download, extract and 
# convert the matrix to the form expected by the accelerator
def prepareUFLMatrix(name):
    f = urllib.URLopener()
    url="http://www.cise.ufl.edu/research/sparse/MM/"+name+".tar.gz"    
    name=name.split("/")[1]
    if not os.path.exists(downloadDir):
        os.makedirs(downloadDir)
    fileName = downloadDir+"/"+name+".tar.gz"
    # download if archive file does not exist
    if not os.path.exists(fileName):
        print "Downloading " + url
        f.retrieve(url, fileName)
    # extract if matrix market file does not exist
    if not os.path.exists(localRoot+"/"+name+".mtx")    :
        print "Extracting matrix..."
        tar = tarfile.open(fileName)
        for item in tar:
            if item.name.endswith(name+".mtx"):
                item.name = name+".mtx"
                print item.name
                tar.extract(item, localRoot)
    # convert if the destination dir doest not exist
    if not os.path.exists(outputBase+"/"+name):
        A=loadMatrix(name)
        convertMatrix(A, name)
        makeGoldenResult(A, name)

# apply fxn to every member of A.data to convert the NZ data value types
def convertDataType(A, fxn):
  Ap = deepcopy(A)
  Ap.data = numpy.array(map(fxn, A.data))
  return Ap

# example of converting data indices to another type (uint64 in this case)
def toUInt64Matrix(A):
    Ap = deepcopy(A)
    Ap.data = numpy.array(map(lambda x: np.uint64(1), A.data))
    return Ap

def makeUnitVector(A):
    return numpy.array([1 for i in range(A.shape[1])])

def makeGoldenResult(A, name):
    x=makeUnitVector(A)
    y=A*x
    f=io.open(outputBase+"/"+name+"/golden.bin", "wb")
    f.write(y.tostring())
    f.close()


# load matrix from local file system (Matrix Market format file must exist
# under localRoot)
def loadMatrix(name):
  name=name.split("/").pop()
  fileName=localRoot+"/"+name+".mtx"
  if os.path.exists(fileName):
    return ios.mmread(fileName).tocsc()
  else:
    print "Matrix not found! " + fileName


def loadAndConvertMatrix(name, startAddr=dramBase):
    A=loadMatrix(name)
    return convertMatrix(A, name, startAddr)

# read in a matrix, convert it to separate CSC SpMV data files + output
# command info (for reading this from an SD card later)
def convertMatrix(A, name, startAddr=dramBase):
  if A.format != "csc":
    print "Matrix must be in CSC format! Converting.."
    A = A.tocsc()
    
  startingRow=0
  startingCol=0
  targetDir=outputBase + "/" + name
  if not os.path.exists(targetDir):
    os.makedirs(targetDir)
  
  # metadata file: information about matrix dimensions
  fileName = targetDir + "/" + name + "-meta.bin"  
  metaDataFile = io.open(fileName, "wb")
  metaDataFile.write(struct.pack("I", A.shape[0]))
  metaDataFile.write(struct.pack("I", A.shape[1]))
  metaDataFile.write(struct.pack("I", A.nnz))
  metaDataFile.write(struct.pack("I", startingRow))
  metaDataFile.write(struct.pack("I", startingCol))
  metaDataFile.write(struct.pack("I", A.indices[0].nbytes))
  metaDataFile.write(struct.pack("I", A.data[0].nbytes))
  metaDataFile.close()

  
  # index pointers
  fileName = targetDir + "/" + name + "-indptr.bin"
  indPtrFile = io.open(fileName, "wb")
  indPtrFile.write(A.indptr.tostring())
  indPtrFile.close()
    
  # indices
  fileName = targetDir + "/" + name + "-inds.bin"
  indsFile = io.open(fileName, "wb")
  indsFile.write(A.indices.tostring())
  indsFile.close()
  
  # nz values
  fileName = targetDir + "/" + name + "-nzdata.bin"
  indsFile = io.open(fileName, "wb")
  indsFile.write(A.data.tostring())
  indsFile.close()  
  
  print "Rows = " + str(A.shape[0])
  print "Cols = " + str(A.shape[1])
  print "NonZ = " + str(A.nnz)
  

