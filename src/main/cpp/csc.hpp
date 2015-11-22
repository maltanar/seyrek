#ifndef CSC_H_
#define CSC_H

#include <string>
#include <vector>

extern void * readMatrixData(std::string name, std::string component);

typedef struct {
  unsigned int rows;
  unsigned int cols;
  unsigned int nz;
  unsigned int startingRow;
  unsigned int startingCol;
  unsigned int bytesPerInd;
  unsigned int bytesPerVal;
} SparseMatrixMetadata;


template <class SpMVInd, class SpMVVal>
class CSC {
public:

  CSC() {
    m_metadata = 0;
    m_indPtrs = 0; m_inds = 0; m_nzData = 0;
    m_name = "<not initialized>";
  }

  virtual ~CSC(){
    if(m_metadata) {
      delete m_metadata;
      delete [] m_indPtrs;
      delete [] m_inds;
      delete [] m_nzData;
    }
  }

  void printSummary() {
    std::cout << "Matrix summary" << std::endl;
    std::cout << "name = " << m_name << std::endl;
    std::cout << "#rows = " << m_metadata->rows << std::endl;
    std::cout << "#cols = " << m_metadata->cols << std::endl;
    std::cout << "#nz = " << m_metadata->nz << std::endl;
  }

  bool isSquare(){
    return m_metadata->cols == m_metadata->rows;
  }

  void setName(std::string name) {m_name = name;}
  std::string getName() {return m_name;}

  static CSC * load(std::string name) {
    SparseMatrixMetadata * md = (SparseMatrixMetadata *)readMatrixData(name, "meta");
    if(md->bytesPerInd != sizeof(SpMVInd)) {
        throw "bytesPerInd mismatch in CSC::load";
    }
    if(md->bytesPerVal != sizeof(SpMVVal)) {
        throw "bytesPerVal mismatch in CSC::load";
    }
    CSC * ret = new CSC();
    ret->m_metadata = md;
    ret->m_indPtrs = (SpMVInd *)readMatrixData(name, "indptr");
    if(!ret->m_indPtrs) throw "could not load indptr in CSC::load";
    ret->m_inds = (SpMVInd *)readMatrixData(name, "inds");
    if(!ret->m_inds) throw "could not load inds in CSC::load";
    ret->m_nzData = (SpMVVal *)readMatrixData(name, "nzdata");
    if(!ret->m_nzData) throw "could not load nzdata in CSC::load";
    ret->m_name = name;

    return ret;
  }

  static CSC * eye(unsigned int dim) {
    CSC * ret = new CSC();
    ret->m_metadata = new SparseMatrixMetadata;
    ret->m_metadata->startingRow = 0;
    ret->m_metadata->startingCol = 0;
    ret->m_metadata->cols = dim;
    ret->m_metadata->rows = dim;
    ret->m_metadata->nz = dim;
    ret->m_indPtrs = new SpMVInd[dim+1];
    ret->m_inds = new SpMVInd[dim];
    ret->m_nzData = new SpMVVal[dim];

    for(SpMVInd i = 0; i < dim; i++) {
        ret->m_indPtrs[i] = i;
        ret->m_inds[i] = i;
        ret->m_nzData[i] = 1; // TODO this should actually come from the semiring
    }
    ret->m_indPtrs[dim] = dim;
    ret->setName("eye");

    return ret;
  }

  unsigned int getCols() const {
    return m_metadata->cols;
  }

  SpMVInd* getIndPtrs() const {
    return m_indPtrs;
  }

  SpMVInd* getInds() const {
    return m_inds;
  }

  unsigned int getNNZ() const {
    return m_metadata->nz;
  }

  SpMVVal* getNZData() const {
    return m_nzData;
  }

  unsigned int getRows() const {
    return m_metadata->rows;
  }

  // given a vector of indices for partition boundaries, returns a vector with the
  // number of elements in each partition. e.g boundaries = {0 10 20}
  // partition 0 will contain elements with i s.t. 0 <= i < 10
  // partition 1 will contain elements with i s.t. 10 <= i < 20
  std::vector<unsigned int> getPartitionElemCnts(std::vector<SpMVInd> boundaries) {
    unsigned int numPartitions = boundaries.size() - 1;
    std::vector<unsigned int> res;
    // initialize all partition counts to zero
    for(unsigned int i=0; i<numPartitions; i++) res.push_back(0);
    // iterate over row indices to determine real counts for each partition
    for(unsigned int i=0; i < m_metadata->nz; i++) {
      SpMVInd currentInd = m_inds[i];
      for(unsigned int p=0; p<numPartitions; p++) {
        if((boundaries[p] <= currentInd) && (currentInd < boundaries[p+1])) {
            res[p] = res[p] + 1;
            break;
        }
      }
    }
    return res;
  }

  // returns a vector with the number of elements per partition, using equidistant
  // partition boundaries (e.g. with the matrix split into equal-sized chunks)
  std::vector<unsigned int> getPartitionElemCnts(unsigned int numPartitions) {
    std::vector<SpMVInd> boundaries;
    unsigned int divSize = (m_metadata->rows + numPartitions) / numPartitions;
    for(unsigned int i = 0; i < numPartitions; i++) {
      boundaries.push_back(divSize * i);
    }
    // push the matrix nz count as the upper bound
    boundaries.push_back(m_metadata->rows);

    return getPartitionElemCnts(boundaries);
  }

/*
  partition the CSC matrix into <numPartitions> chunks (sliced along rows)
  std::vector<CSC<SpMVInd, SpMVVal> > partition(unsigned int numPartitions) {

  }

  std::vector<CSC<SpMVInd, SpMVVal> > partition(std::vector<SpMVInd> boundaries) {

  }
*/


protected:
  SparseMatrixMetadata * m_metadata;

  SpMVInd * m_indPtrs;
  SpMVInd * m_inds;
  SpMVVal * m_nzData;
  std::string m_name;

};

#endif
