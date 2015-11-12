#include <iostream>
#include "swcscspmv.hpp"
#include "hwcscspmv.hpp"
#include "commonsemirings.hpp"
#include "platform.h"
#include <string.h>

using namespace std;

typedef unsigned int SpMVInd;
typedef int SpMVVal;

#include <stdio.h>

void * readMatrixData(std::string name, std::string component) {
  string matricesBase = "/home/maltanar/seyrek/matrices";
  string fileName = matricesBase + "/" + name + "/" + name + "-" + component + ".bin";
  FILE *f = fopen(fileName.c_str(), "rb");
  if(!f) throw (string("Could not open file: ") + fileName).c_str();
  fseek(f, 0, SEEK_END);
  unsigned int fsize = ftell(f);
  fseek(f, 0, SEEK_SET);

  void * buf = new char[fsize];
  unsigned int r = fread(buf, 1, fsize, f);

  if(r != fsize) throw "Read error";

  fclose(f);

  return buf;
}



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
    HWSpMV<SpMVInd, SpMVVal> * hw = new HWSpMV<SpMVInd, SpMVVal>("SampleSpMV", platform);

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

