#ifndef CSC_H_
#define CSC_H

#include <string>

template <class SpMVInd, class SpMVVal>
class CSC {
public:
  CSC() {
    m_nz = 0; m_cols = 0; m_rows = 0;
    m_startingCol = 0; m_startingRow = 0;
    m_indPtrs = 0; m_inds = 0; m_nzData = 0;
    m_name = "<not initialized>";
  }

  virtual ~CSC(){
    if(m_nz) {
      delete [] m_indPtrs;
      delete [] m_inds;
      delete [] m_nzData;
    }
  }

  void printSummary() {
    std::cout << "Matrix summary" << std::endl;
    std::cout << "name = " << m_name << std::endl;
    std::cout << "#rows = " << m_rows << std::endl;
    std::cout << "#cols = " << m_cols << std::endl;
    std::cout << "#nz = " << m_nz << std::endl;
  }

  bool isSquare(){
    return m_cols == m_rows;
  }

  void setName(std::string name) {m_name = name;};
  std::string getName() {return m_name;};

  static CSC * eye(unsigned int dim) {
    CSC * ret = new CSC();
    ret->m_startingRow = 0;
    ret->m_startingCol = 0;
    ret->m_cols = dim;
    ret->m_rows = dim;
    ret->m_nz = dim;
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
    return m_cols;
  }

  SpMVInd* getIndPtrs() const {
    return m_indPtrs;
  }

  SpMVInd* getInds() const {
    return m_inds;
  }

  unsigned int getNz() const {
    return m_nz;
  }

  SpMVVal* getNzData() const {
    return m_nzData;
  }

  unsigned int getRows() const {
    return m_rows;
  }

protected:
  unsigned int m_rows;
  unsigned int m_cols;
  unsigned int m_nz;
  unsigned int m_startingRow;
  unsigned int m_startingCol;
  SpMVInd * m_indPtrs;
  SpMVInd * m_inds;
  SpMVVal * m_nzData;
  std::string m_name;

};

#endif
