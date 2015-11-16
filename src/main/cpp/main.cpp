#include <iostream>
#include "swcscspmv.hpp"
#include "hwcscspmv.hpp"
#include "commonsemirings.hpp"
#include "platform.h"
#include <string.h>

using namespace std;

typedef unsigned int SpMVInd;
typedef int SpMVVal;


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
  try {

    CSC<SpMVInd, SpMVVal> * A = CSC<SpMVInd, SpMVVal>::load("circuit204-int");
    SpMVVal * x = new SpMVVal[A->getCols()];
    SpMVVal * y = new SpMVVal[A->getRows()];
    for(int i = 0; i < A->getRows(); i++) {
        x[i] = 1;
        y[i] = 0;
    }

    WrapperRegDriver * platform = initPlatform();
    HWSpMV<SpMVInd, SpMVVal> * hw = new HWSpMV<SpMVInd, SpMVVal>("UInt32BRAMSpMV", platform);

    hw->setA(A);
    hw->setx(x);
    hw->sety(y);

    hw->exec();

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

    delete hw;
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

