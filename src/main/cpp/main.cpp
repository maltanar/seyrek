#include <iostream>
#include "swcscspmv.hpp"
#include "commonsemirings.hpp"

using namespace std;

typedef unsigned int SpMVInd;
typedef unsigned int SpMVVal;

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
  cout << "Hello World!" << endl;
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
}

