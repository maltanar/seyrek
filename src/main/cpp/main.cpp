#include <iostream>
#include "swcscspmv.hpp"
#include "commonsemirings.hpp"

using namespace std;

typedef unsigned int SpMVInd;
typedef unsigned int SpMVVal;

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



// little demo of how Seyrek's software side semiring comps might look like

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
    cout << "Hello World!" << endl;
    CSC<SpMVInd, SpMVVal> * B = CSC<SpMVInd, SpMVVal>::load("circuit204-int");
    RegSpMV t;

    CSC<SpMVInd, SpMVVal> * A = CSC<SpMVInd, SpMVVal>::eye(10);
    SpMVVal * x = new SpMVVal[10];
    SpMVVal * y = new SpMVVal[10];
    for(int i = 0; i < 10; i++) {
        x[i] = 1;
        y[i] = 0;
      }

    t.setA(A);
    t.setx(x);
    t.sety(y);

    t.exec();

    for(unsigned int i = 0; i < 10; i++) {
        cout << i << " " << y[i] << endl;
    }

    delete A;
    delete x;
    delete y;

    return 0;

  } catch(char const * err) {
    cerr << "Exception: " << err << endl;
    return 1;
  }

}

