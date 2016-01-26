#include <iostream>
#include "SWCSRSpMV.hpp"
#include "HWCSRSpMV.hpp"
#include "commonsemirings.hpp"
#include "platform.h"
#include <string.h>

using namespace std;

typedef unsigned int SpMVInd;
typedef int64_t SpMVVal;


class RegSpMV: public AddMulSemiring<SpMVInd, SpMVVal>, public SWCSRSpMV<SpMVInd, SpMVVal> {
public:
  virtual unsigned int statInt(std::string name) { return 0;}

  virtual std::vector<std::string> statKeys() {
    vector<string> keys;
    keys.push_back("matrix");
  }
};

int main(int argc, char *argv[])
{
  typedef CSR<SpMVInd, SpMVVal> SparseMatrix;
  typedef HWCSRSpMV<SpMVInd, SpMVVal> HardwareSpMV;

  try {
    string matrixName;
    cout << "Enter matrix name: " << endl;
    cin >> matrixName;

    SparseMatrix * A;
    unsigned int dim = 0;
    if (matrixName == "eye") {
      cout << "Enter dimension for identity matrix: " << endl;
      cin >> dim;
      A = SparseMatrix::eye(dim);
    } else if (matrixName == "dense") {
      cout << "Enter dimension for dense matrix: " << endl;
      cin >> dim;
      A = SparseMatrix::dense(dim);
    } else
      A = SparseMatrix::load(matrixName);

    A->printSummary();
    SpMVVal * x = new SpMVVal[A->getCols()];
    SpMVVal * y = new SpMVVal[A->getRows()];
    for(int i = 0; i < A->getRows(); i++) {
        x[i] = (i+1)*10;
        y[i] = 0;
    }


    WrapperRegDriver * platform = initPlatform();
    string attachname;
    cout << "Enter attach name: " << endl;
    cin >> attachname;

    HardwareSpMV * acc = new HardwareSpMV(platform, 0, attachname.c_str());

    cout << "Setting inputs..." << endl;

    acc->setA(A);
    acc->setx(x);
    acc->sety(y);

    cout << "Executing..." << endl;


    acc->exec();

    acc->printAllStats();

    cout << "Completed, checking result..." << endl;

    RegSpMV chk;
    chk.setA(A);
    chk.setx(x);
    SpMVVal * goldeny = new SpMVVal[A->getRows()];
    for(int i = 0; i < A->getRows(); i++) {
        goldeny[i] = 0;
    }
    chk.sety(goldeny);
    chk.exec();
    int res = memcmp(y, goldeny, A->getRows() * sizeof(SpMVVal));
    cout << "memcmp result: " << res << endl;

    if(res != 0) {
      cout << "Result has errors, print comparison? (y/n)" << endl;
      char yn;
      cin >> yn;
      if(yn == 'y') {
        for(int i = 0; i < A->getRows(); i++) {
          if(goldeny[i] != y[i]) cout << i << " golden: " << goldeny[i] << " res: " << y[i] << endl;
        }
      }
    }
    

    delete acc;
    delete [] x;
    delete [] y;
    delete [] goldeny;
    delete A;


    deinitPlatform(platform);

    return 0;

  } catch(char const * err) {
    cerr << "Exception: " << err << endl;
    return 1;
  }

}
