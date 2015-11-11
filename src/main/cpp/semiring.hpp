#ifndef SEMIRING_HPP
#define SEMIRING_HPP

// abstract base class template for defining semiring add and mul
// note that the op coordinates are also passed to the implementations,
// if the user wants to specialize the operations based on coordinate
// TODO also expose semiring's own 0 and 1 definitions

template <class SpMVInd, class SpMVVal>
class Semiring {
public:
  virtual ~Semiring() {};
protected:
  virtual SpMVVal mul(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) = 0;
  virtual SpMVVal add(SpMVVal first, SpMVVal second, SpMVInd row, SpMVInd col) = 0;
};

#endif // SEMIRING_HPP
