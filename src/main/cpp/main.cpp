#include <iostream>
#include "swcscspmv.hpp"
#include "hwcscspmv.hpp"
#include "commonsemirings.hpp"
#include "platform.h"
#include <string.h>
#include "parallelspmv.hpp"

using namespace std;

typedef unsigned int SpMVInd;
typedef int64_t SpMVVal;


class RegSpMV: public AddMulSemiring<SpMVInd, SpMVVal>, public SWSpMV<SpMVInd, SpMVVal> {
public:
  virtual unsigned int statInt(std::string name) { return 0;}

  virtual std::vector<std::string> statKeys() {
    vector<string> keys;
    keys.push_back("matrix");
  }
};

int main(int argc, char *argv[])
{
  typedef CSC<SpMVInd, SpMVVal> SparseMatrix;
  typedef HWSpMV<SpMVInd, SpMVVal> HardwareSpMV;
  typedef ParallelHWSpMV<SpMVInd, SpMVVal> ParSpMV;

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
        x[i] = 1;
        y[i] = 0;
    }


    WrapperRegDriver * platform = initPlatform();
    string attachname;
    cout << "Enter attach name: " << endl;
    cin >> attachname;

    ParSpMV * par = new ParSpMV(1, platform, attachname.c_str());

    cout << "Setting inputs..." << endl;

    par->setA(A);
    par->setx(x);
    par->sety(y);

    cout << "Executing..." << endl;


    par->exec();

    par->getPE(0)->printAllStats();

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

    if(res != 0)
      for(int i = 0; i < A->getRows(); i++) {
        if(goldeny[i] != y[i]) cout << i << " golden: " << goldeny[i] << " res: " << y[i] << endl;
      }

    delete par;
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

