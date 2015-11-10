#include <iostream>
#include "swcscspmv.hpp"
#include "commonsemirings.hpp"

using namespace std;

typedef unsigned int SpMVInd;
typedef unsigned int SpMVVal;

// little demo of how Seyrek's software side semiring comps might look like

class RegSpMV: public AddMulSemiring<SpMVInd, SpMVVal>, public SWSpMV<SpMVInd, SpMVVal> {
public:
  RegSpMV() {
    m_cols = 10;
    m_rows = 10;
    m_nnz = 10;
    m_colPtr = new SpMVInd[m_cols+1];
    m_rowInd = new SpMVInd[m_nnz];
    m_nzData = new SpMVVal[m_nnz];
    m_x = new SpMVVal[m_rows];
    m_y = new SpMVVal[m_cols];
    for(SpMVInd i = 0; i < m_nnz; i++) {
      m_colPtr[i] = i;
      m_rowInd[i] = i;
      m_nzData[i] = 1;
      m_x[i] = i+1;
      m_y[i] = 0;
    }
    m_colPtr[m_cols] = m_cols;
  }

  ~RegSpMV() {
    delete [] m_colPtr;
    delete [] m_nzData;
    delete [] m_rowInd;
    delete [] m_x;
    delete [] m_y;
  }

  virtual unsigned int statInt(std::string name) { return 0;}

  virtual std::vector<std::string> statKeys() {
    vector<string> keys;
    keys.push_back("matrix");
  }
};

int main(int argc, char *argv[])
{
  cout << "Hello World!" << endl;
  RegSpMV t;

  t.exec();
  SpMVVal * y = t.getY();
  for(unsigned int i = 0; i < 10; i++) {
      cout << i << " " << y[i] << endl;
  }
  return 0;
}

